;;; appkit-topic.el --- App-to-Surface publish and subscribe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-apps/appkit.el

;;; Commentary:

;; An App publishes payloads on topics after it commits; its attached Surfaces
;; declare the topics they follow from their own committed models.  Each
;; notification reaches the exact subscribing Surface incarnation through that
;; Surface's ordinary update, in its data lane, so it keeps FIFO order with
;; directed replies from the same App pass.
;;
;; Level subscriptions keep at most one pending notification per topic and
;; replace its payload: payloads are hints, and the Surface rebuilds from its
;; App read view.  Lossless subscriptions keep every payload in order up to a
;; bound, then discard the backlog and deliver one resync message.  A full data
;; lane moves the notification to one coalesced control wake per Surface.
;;
;; The App's hub also reports when a topic gains its first or loses its last
;; subscriber, so canonical state can follow interest without a watch table.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)
(require 'appkit-source)

(defconst appkit-topic-default-subscription-limit 16
  "Default maximum number of topics one Surface may follow.")

(defconst appkit-topic-stale (make-symbol "appkit-topic-stale")
  "Internal result for a notification that lost its subscription.")

(cl-defstruct (appkit-subscription
               (:constructor appkit-subscription--create)
               (:copier nil))
  "One Surface subscription to an App topic."
  topic
  notify
  delivery
  pending-limit
  resync)

(cl-defun appkit-subscription-create
    (&key topic notify (delivery 'level) pending-limit resync)
  "Return a subscription to TOPIC, compared with `equal'.

NOTIFY maps one published payload to the message delivered to the Surface's
update.  DELIVERY `level' keeps only the latest undelivered payload; it is a
hint, and the Surface rebuilds from its App read view.  DELIVERY `lossless'
delivers every payload in order.  When more than PENDING-LIMIT payloads are
undelivered, they are discarded and RESYNC's zero-argument message is delivered
instead."
  (unless topic (error "Subscription topic must be non-nil"))
  (unless (functionp notify)
    (signal 'wrong-type-argument (list 'functionp notify)))
  (pcase delivery
    ('level
     (when (or pending-limit resync)
       (error "Level subscriptions take no pending limit or resync")))
    ('lossless
     (unless (and (integerp pending-limit) (> pending-limit 0))
       (error "Lossless subscription requires a positive pending limit"))
     (unless (functionp resync)
       (error "Lossless subscription requires a resync function")))
    (_ (error "Unsupported subscription delivery: %S" delivery)))
  (appkit-subscription--create
   :topic topic
   :notify notify
   :delivery delivery
   :pending-limit pending-limit
   :resync resync))

(cl-defstruct (appkit-topic--hub
               (:constructor appkit-topic--hub-create)
               (:copier nil))
  "Topic subscriptions and interest reporting for one App incarnation."
  loop
  subscribed
  records
  delivered
  pending
  pending-order
  wake-p
  alive-p)

(cl-defstruct (appkit-topic--subscriber
               (:constructor appkit-topic--subscriber-create)
               (:copier nil))
  "One Surface incarnation's subscriptions."
  hub
  loop
  incarnation
  records
  limit
  overflow
  wake-p
  alive-p)

(cl-defstruct (appkit-topic--record
               (:constructor appkit-topic--record-create)
               (:copier nil))
  "One live subscription of one subscriber to one topic.
QUEUED is a level delivery still in the data lane.  OVERFLOW is nil,
`(level . PAYLOAD)' or `resync' while waiting behind the control wake."
  subscriber
  topic
  subscription
  queued
  (pending-count 0)
  (generation 0)
  overflow
  active-p)

(cl-defstruct (appkit-topic--delivery
               (:constructor appkit-topic--delivery-create)
               (:copier nil))
  record
  generation
  payload)

(cl-defstruct (appkit-topic--wake
               (:constructor appkit-topic--wake-create)
               (:copier nil))
  subscriber)

(cl-defstruct (appkit-topic--interest-wake
               (:constructor appkit-topic--interest-wake-create)
               (:copier nil))
  hub)

(defun appkit-topic-message-p (message)
  "Return non-nil when MESSAGE is an internal Surface topic envelope."
  (or (appkit-topic--delivery-p message) (appkit-topic--wake-p message)))

(defun appkit-topic-interest-message-p (message)
  "Return non-nil when MESSAGE is an internal App interest wake."
  (appkit-topic--interest-wake-p message))

;;; Hub

(defun appkit-topic-hub-create (loop subscribed)
  "Create the topic hub of the App running on LOOP.
SUBSCRIBED, when non-nil, maps (TOPIC SUBSCRIBED-P) to an App message, or nil
to ignore the change."
  (unless (or (null subscribed) (functionp subscribed))
    (signal 'wrong-type-argument (list 'functionp subscribed)))
  (appkit-topic--hub-create
   :loop loop
   :subscribed subscribed
   :records (make-hash-table :test #'equal)
   :delivered (make-hash-table :test #'equal)
   :pending (make-hash-table :test #'equal)
   :pending-order nil
   :wake-p nil
   :alive-p t))

(defun appkit-topic-hub-stop (hub)
  "Stop HUB; later publications and interest changes are inert."
  (when (appkit-topic--hub-p hub)
    (setf (appkit-topic--hub-alive-p hub) nil
          (appkit-topic--hub-pending-order hub) nil)
    (clrhash (appkit-topic--hub-records hub))
    (clrhash (appkit-topic--hub-pending hub))
    (clrhash (appkit-topic--hub-delivered hub))))

(defun appkit-topic--ensure-interest-wake (hub)
  "Post HUB's one coalesced interest wake when changes are pending."
  (when (and (appkit-topic--hub-alive-p hub)
             (appkit-topic--hub-pending-order hub)
             (not (appkit-topic--hub-wake-p hub)))
    (let* ((loop (appkit-topic--hub-loop hub))
           (outcome
            (appkit-loop--post-control-addressed
             loop (appkit-topic--interest-wake-create :hub hub)
             (appkit-loop-incarnation loop))))
      (pcase outcome
        ('enqueued (setf (appkit-topic--hub-wake-p hub) t))
        ('full
         (appkit-loop--enter-fault
          loop '(error "Topic interest control lane is full") nil))))))

(defun appkit-topic--interest-changed (hub topic)
  "Schedule HUB's report for TOPIC unless the App already knows its state."
  (when (and (appkit-topic--hub-alive-p hub)
             (appkit-topic--hub-subscribed hub))
    (let ((subscribed-p (and (gethash topic (appkit-topic--hub-records hub)) t))
          (pending (appkit-topic--hub-pending hub)))
      (cond
       ((eq subscribed-p (gethash topic (appkit-topic--hub-delivered hub)))
        ;; A reversal before delivery is no change from the App's view.
        (when (gethash topic pending)
          (remhash topic pending)
          (setf (appkit-topic--hub-pending-order hub)
                (delete topic (appkit-topic--hub-pending-order hub)))))
       ((not (gethash topic pending))
        (puthash topic t pending)
        (setf (appkit-topic--hub-pending-order hub)
              (append (appkit-topic--hub-pending-order hub) (list topic)))
        (appkit-topic--ensure-interest-wake hub))))))

(defun appkit-topic-consume-interest (hub wake)
  "Consume WAKE and return HUB's next interest message.
Return nil when the App ignores the change, or `appkit-topic-stale' when WAKE
no longer belongs to a live HUB."
  (if (not (and (eq hub (appkit-topic--interest-wake-hub wake))
                (appkit-topic--hub-alive-p hub)))
      appkit-topic-stale
    (setf (appkit-topic--hub-wake-p hub) nil)
    (let ((topic (pop (appkit-topic--hub-pending-order hub))))
      (if (null topic)
          appkit-topic-stale
        (remhash topic (appkit-topic--hub-pending hub))
        (let ((subscribed-p
               (and (gethash topic (appkit-topic--hub-records hub)) t)))
          (if subscribed-p
              (puthash topic t (appkit-topic--hub-delivered hub))
            (remhash topic (appkit-topic--hub-delivered hub)))
          (appkit-topic--ensure-interest-wake hub)
          (funcall (appkit-topic--hub-subscribed hub) topic subscribed-p))))))

;;; Subscribers

(defun appkit-topic-subscriber-create (hub loop limit)
  "Create the subscriber for the Surface running on LOOP under HUB.
LIMIT bounds the number of topics it may follow."
  (unless (and (integerp limit) (> limit 0))
    (error "Subscription limit must be positive: %S" limit))
  (appkit-topic--subscriber-create
   :hub hub
   :loop loop
   :incarnation (appkit-loop-incarnation loop)
   :records (make-hash-table :test #'equal)
   :limit limit
   :overflow nil
   :wake-p nil
   :alive-p t))

(defun appkit-topic--subscribe (subscriber subscription)
  "Start SUBSCRIBER's record for SUBSCRIPTION."
  (let* ((hub (appkit-topic--subscriber-hub subscriber))
         (topic (appkit-subscription-topic subscription))
         (record (appkit-topic--record-create
                  :subscriber subscriber
                  :topic topic
                  :subscription subscription
                  :active-p t)))
    (puthash topic record (appkit-topic--subscriber-records subscriber))
    (puthash topic
             (append (gethash topic (appkit-topic--hub-records hub))
                     (list record))
             (appkit-topic--hub-records hub))
    (appkit-topic--interest-changed hub topic)))

(defun appkit-topic--unsubscribe (record)
  "End RECORD; its undelivered notifications become stale."
  (let* ((subscriber (appkit-topic--record-subscriber record))
         (hub (appkit-topic--subscriber-hub subscriber))
         (topic (appkit-topic--record-topic record))
         (remaining (remq record (gethash topic (appkit-topic--hub-records hub)))))
    (setf (appkit-topic--record-active-p record) nil
          (appkit-topic--record-queued record) nil
          (appkit-topic--record-overflow record) nil
          (appkit-topic--record-generation record)
          (1+ (appkit-topic--record-generation record))
          (appkit-topic--subscriber-overflow subscriber)
          (delq record (appkit-topic--subscriber-overflow subscriber)))
    (remhash topic (appkit-topic--subscriber-records subscriber))
    (if remaining
        (puthash topic remaining (appkit-topic--hub-records hub))
      (remhash topic (appkit-topic--hub-records hub)))
    (appkit-topic--interest-changed hub topic)))

(defun appkit-topic-reconcile (subscriber subscriptions)
  "Make SUBSCRIBER follow exactly the desired SUBSCRIPTIONS.
An unchanged topic keeps its pending notifications.  Changing a topic's
delivery mode restarts it."
  (unless (appkit-topic--subscriber-alive-p subscriber)
    (error "Cannot reconcile a detached topic subscriber"))
  (unless (appkit-source--proper-bounded-list-p
           subscriptions (appkit-topic--subscriber-limit subscriber))
    (error "Subscription limit exceeded"))
  (let ((desired (make-hash-table :test #'equal))
        (records (appkit-topic--subscriber-records subscriber))
        gone)
    (dolist (subscription subscriptions)
      (unless (appkit-subscription-p subscription)
        (signal 'wrong-type-argument
                (list 'appkit-subscription-p subscription)))
      (let ((topic (appkit-subscription-topic subscription)))
        (when (gethash topic desired)
          (error "Subscriptions duplicate topic %S" topic))
        (puthash topic subscription desired)))
    (maphash (lambda (topic record)
               (unless (gethash topic desired) (push record gone)))
             records)
    (mapc #'appkit-topic--unsubscribe gone)
    (dolist (subscription subscriptions)
      (let ((record (gethash (appkit-subscription-topic subscription) records)))
        (cond
         ((null record)
          (appkit-topic--subscribe subscriber subscription))
         ((eq (appkit-subscription-delivery subscription)
              (appkit-subscription-delivery
               (appkit-topic--record-subscription record)))
          (setf (appkit-topic--record-subscription record) subscription))
         (t
          (appkit-topic--unsubscribe record)
          (appkit-topic--subscribe subscriber subscription)))))))

(defun appkit-topic-detach (subscriber)
  "End all of SUBSCRIBER's subscriptions."
  (when (and (appkit-topic--subscriber-p subscriber)
             (appkit-topic--subscriber-alive-p subscriber))
    (setf (appkit-topic--subscriber-alive-p subscriber) nil)
    (let (records)
      (maphash (lambda (_topic record) (push record records))
               (appkit-topic--subscriber-records subscriber))
      (mapc #'appkit-topic--unsubscribe records))
    (setf (appkit-topic--subscriber-overflow subscriber) nil
          (appkit-topic--subscriber-wake-p subscriber) nil)))

;;; Publication

(defun appkit-topic--ensure-wake (subscriber)
  "Post SUBSCRIBER's one coalesced overflow wake when overflow is pending."
  (when (and (appkit-topic--subscriber-alive-p subscriber)
             (appkit-topic--subscriber-overflow subscriber)
             (not (appkit-topic--subscriber-wake-p subscriber)))
    (let* ((loop (appkit-topic--subscriber-loop subscriber))
           (outcome
            (appkit-loop--post-control-addressed
             loop (appkit-topic--wake-create :subscriber subscriber)
             (appkit-topic--subscriber-incarnation subscriber))))
      (pcase outcome
        ('enqueued (setf (appkit-topic--subscriber-wake-p subscriber) t))
        ('full
         (appkit-loop--enter-fault
          loop '(error "Topic overflow control lane is full") nil))))))

(defun appkit-topic--overflow (record entry)
  "Hold ENTRY for RECORD behind its subscriber's control wake."
  (let ((subscriber (appkit-topic--record-subscriber record)))
    (unless (appkit-topic--record-overflow record)
      (setf (appkit-topic--subscriber-overflow subscriber)
            (append (appkit-topic--subscriber-overflow subscriber)
                    (list record))))
    (setf (appkit-topic--record-overflow record) entry)
    (appkit-topic--ensure-wake subscriber)))

(defun appkit-topic--resync (record)
  "Discard RECORD's lossless backlog and schedule its resync message."
  (setf (appkit-topic--record-generation record)
        (1+ (appkit-topic--record-generation record))
        (appkit-topic--record-pending-count record) 0)
  (appkit-topic--overflow record 'resync))

(defun appkit-topic--admit (record payload source-address revision)
  "Admit PAYLOAD for RECORD to its Surface data lane; return the outcome."
  (let* ((subscriber (appkit-topic--record-subscriber record))
         (delivery (appkit-topic--delivery-create
                    :record record
                    :generation (appkit-topic--record-generation record)
                    :payload payload))
         (outcome
          (appkit-loop--post-addressed
           (appkit-topic--subscriber-loop subscriber) delivery
           (appkit-topic--subscriber-incarnation subscriber)
           nil 'publish source-address revision)))
    (cons outcome delivery)))

(defun appkit-topic--offer (record payload source-address revision)
  "Deliver PAYLOAD to one RECORD according to its delivery mode."
  (let ((subscription (appkit-topic--record-subscription record)))
    (pcase (appkit-subscription-delivery subscription)
      ('level
       (cond
        ((appkit-topic--record-queued record)
         (setf (appkit-topic--delivery-payload (appkit-topic--record-queued record))
               payload))
        ((appkit-topic--record-overflow record)
         (setf (appkit-topic--record-overflow record) (cons 'level payload)))
        (t
         (pcase-let ((`(,outcome . ,delivery)
                      (appkit-topic--admit record payload source-address revision)))
           (pcase outcome
             ('enqueued (setf (appkit-topic--record-queued record) delivery))
             ('full (appkit-topic--overflow record (cons 'level payload))))))))
      ('lossless
       (cond
        ((appkit-topic--record-overflow record))
        ((>= (appkit-topic--record-pending-count record)
             (appkit-subscription-pending-limit subscription))
         (appkit-topic--resync record))
        (t
         (pcase (car (appkit-topic--admit record payload source-address revision))
           ('enqueued
            (setf (appkit-topic--record-pending-count record)
                  (1+ (appkit-topic--record-pending-count record))))
           ('full (appkit-topic--resync record)))))))))

(defun appkit-topic-publish (hub topic payload source-address revision)
  "Offer PAYLOAD on TOPIC to every current subscription in HUB.
SOURCE-ADDRESS and REVISION stamp the App commit that published it."
  (when (appkit-topic--hub-alive-p hub)
    (dolist (record (copy-sequence (gethash topic (appkit-topic--hub-records hub))))
      (when (and (appkit-topic--record-active-p record)
                 (appkit-topic--subscriber-alive-p
                  (appkit-topic--record-subscriber record)))
        (appkit-topic--offer record payload source-address revision)))))

;;; Consumption

(defun appkit-topic--notify (record payload)
  "Return RECORD's mapped message for PAYLOAD."
  (funcall (appkit-subscription-notify (appkit-topic--record-subscription record))
           payload))

(defun appkit-topic--consume-wake (subscriber wake)
  "Return the message for SUBSCRIBER's next overflow entry behind WAKE."
  (if (not (eq subscriber (appkit-topic--wake-subscriber wake)))
      appkit-topic-stale
    (setf (appkit-topic--subscriber-wake-p subscriber) nil)
    (let* ((record (pop (appkit-topic--subscriber-overflow subscriber)))
           (entry (and record (appkit-topic--record-overflow record))))
      (when record
        (setf (appkit-topic--record-overflow record) nil))
      (appkit-topic--ensure-wake subscriber)
      (cond
       ((null entry) appkit-topic-stale)
       ((eq entry 'resync)
        (funcall (appkit-subscription-resync
                  (appkit-topic--record-subscription record))))
       (t (appkit-topic--notify record (cdr entry)))))))

(defun appkit-topic-consume (subscriber message)
  "Return the Surface message carried by topic MESSAGE for SUBSCRIBER.
Return `appkit-topic-stale' when its subscription has ended or changed."
  (if (appkit-topic--wake-p message)
      (appkit-topic--consume-wake subscriber message)
    (let ((record (appkit-topic--delivery-record message)))
      (if (not (and (appkit-topic--record-active-p record)
                    (eq subscriber (appkit-topic--record-subscriber record))
                    (= (appkit-topic--delivery-generation message)
                       (appkit-topic--record-generation record))))
          appkit-topic-stale
        (pcase (appkit-subscription-delivery
                (appkit-topic--record-subscription record))
          ('level
           (if (not (eq message (appkit-topic--record-queued record)))
               appkit-topic-stale
             (setf (appkit-topic--record-queued record) nil)
             (appkit-topic--notify record (appkit-topic--delivery-payload message))))
          ('lossless
           (setf (appkit-topic--record-pending-count record)
                 (1- (appkit-topic--record-pending-count record)))
           (appkit-topic--notify record
                                 (appkit-topic--delivery-payload message))))))))

(provide 'appkit-topic)

;;; appkit-topic.el ends here
