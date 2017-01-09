#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defclass client (lichat-tcp-client:client)
  ((main :initarg :main :accessor main)
   (send-queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor send-queue)
   (queue-lock :initform (bt:make-lock) :accessor queue-lock)
   (send-thread :initform NIL :accessor send-thread)))

(defmethod open-connection ((client client))
  (lichat-tcp-client:open-connection client)
  (setf (send-thread client)
        (bt:make-thread (lambda ()
                          (unwind-protect
                               (handle-send-connection client)
                            (setf (send-thread client) NIL))))))

(defmethod close-connection ((client client))
  (lichat-tcp-client:close-connection client))

(defmethod find-channel (name (client client))
  (find-channel name (main client)))

(defmethod (setf find-channel) (value name (client client))
  (setf (find-channel name (main client)) value))

(defmethod handle-send-connection ((client client))
  (loop while (ignore-errors (open-stream-p (lichat-tcp-client::socket-stream client)))
        for queue = (send-queue client)
        do (cond ((= 0 (length queue))
                  (sleep 0.1))
                 (T
                  (v:info :lionchat.client "Found messages to send.")
                  (bt:with-lock-held ((queue-lock client))
                    (setf (send-queue client) (make-array 0 :adjustable T :fill-pointer T)))
                  (loop for update across queue
                        do (lichat-tcp-client:send update client))))))

(defmethod lichat-tcp-client:process ((update lichat-protocol:failure) (client client))
  (let ((channel (find-channel (lichat-protocol:from update) client)))
    (update channel update)))

(defmethod lichat-tcp-client:process ((update lichat-protocol:channel-update) (client client))
  (let ((channel (find-channel (lichat-protocol:channel update) client)))
    (update channel update)))

(defmethod lichat-tcp-client:process ((update lichat-protocol:join) (client client))
  (when (string= (lichat-tcp-client:name client)
               (lichat-protocol:from update))
    (setf (find-channel (lichat-protocol:channel update) client)
          (make-instance 'channel :name (lichat-protocol:channel update))))
  (update (find-channel (lichat-protocol:channel update) client)
          update))

(defmethod lichat-tcp-client:process ((update lichat-protocol:leave) (client client))
  (update (find-channel (lichat-protocol:channel update) client)
          update)
  (when (string= (lichat-tcp-client:name client)
                 (lichat-protocol:from update))
    (setf (find-channel (lichat-protocol:channel update) client)
          NIL)))

(defmethod enqueue-for-sending ((update lichat-protocol:update) (client client))
  (bt:with-lock-held ((queue-lock client))
    (vector-push-extend update (send-queue client))))

(defun qsend (client type &rest initargs)
  (enqueue-for-sending
   (apply #'make-instance type
          :from (lichat-tcp-client:name client)
          initargs)
   client))
