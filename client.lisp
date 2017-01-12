#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

;; Redef to add client slot
(lichat-protocol:define-protocol-class lichat-protocol:wire-object ()
  ((client :initform NIL :accessor client :slot-type (or null client))))

(defclass client (lichat-tcp-client:client updatable)
  ((main :initarg :main :accessor main)
   (name :initarg :name :accessor name)
   (server-name :initform NIL :accessor server-name)
   (send-thread :initform NIL :accessor send-thread)
   (users :initform NIL :accessor users)
   (channels :initform NIL :accessor channels))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod initialize-instance :before ((client client) &key name)
  (when (string= name "")
    (error "NAME cannot be empty.")))

(defmethod client ((client client))
  client)

(defmethod open-connection :after ((client client))
  (setf (send-thread client)
        (bt:make-thread (lambda ()
                          (unwind-protect
                               (handle-send-connection client)
                            (setf (send-thread client) NIL)))))
  client)

(defmethod close-connection :before ((client client))
  (when (and (channel (main client))
             (eql client (client (channel (main client)))))
    (setf (channel (main client)) NIL))
  (setf (find-client (name client) (main client)) NIL))

(defmethod handle-send-connection ((client client))
  (loop while (ignore-errors (open-stream-p (lichat-tcp-client::socket-stream client)))
        do (unless (process-updates client)
             (sleep 0.01))))

(defmethod update ((client client) (update lichat-protocol:update))
  (send update client))

(defun qsend (client type &rest initargs)
  (let ((client (client client)))
    (enqueue-update
     (apply #'make-instance type
            :from (username client)
            initargs)
     client)))

(defmethod find-user (name (client client))
  (find name (users client) :key #'name :test #'string-equal))

(defmethod (setf find-user) (value name (client client))
  (setf-named (users client) name value))

(defmethod find-channel (name (client client))
  (if (eql name T)
      (find T (channels client) :key #'primary-p)
      (find name (channels client) :key #'name :test #'string-equal)))

(defmethod (setf find-channel) (value name (client client))
  (setf-named (channels client) name value))

(defmethod process :before ((update lichat-protocol:wire-object) (client client))
  (setf (client update) client))

(defmethod process ((update lichat-protocol:connect) (client client))
  (setf (server-name client) (lichat-protocol:from update)))

(defmethod process ((update lichat-protocol:users) (client client))
  (when (primary-p (find-channel (lichat-protocol:channel update) client))
    (dolist (name (lichat-protocol:users update))
      (unless (find-user name client)
        (setf (find-user name client)
              (make-instance 'user :name name :client client)))))
  (process update channel))

(defmethod process ((update lichat-protocol:join) (client client))
  (let ((channel (lichat-protocol:channel update)))
    (when (string= (username client) (lichat-protocol:from update))
      (setf (find-channel channel client)
            (make-instance 'channel :name channel
                                    :client client))
      ;; Get user listing for the new channel.
      (qsend client 'lichat-protocol:users :channel channel))
    (process update channel)
    (when (primary-p (find-channel channel client))
      (setf (find-user (lichat-protocol:from update) client)
            (make-instance 'user :name (lichat-protocol:from update) :client client)))))

(defmethod process ((update lichat-protocol:leave) (client client))
  (let ((channel (lichat-protocol:channel update)))
    (when (primary-p (find-channel channel client))
      (setf (find-user (lichat-protocol:from update) client) NIL))
    (process update channel)
    (when (string= (username client) (lichat-protocol:from update))
      (setf (find-channel channel client)
            NIL))))

(defmethod process ((update lichat-protocol:channel-update) (client client))
  (process update (find-channel (lichat-protocol:channel update) client)))

;; Deliver to main thread for synchronised processing, make sure it happens
;; after we're done updating all the internal objects.
(defmethod process :around ((update lichat-protocol:update) (client client))
  (call-next-method)
  (enqueue-update update (main client)))
