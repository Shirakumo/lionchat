#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defclass client (lichat-tcp-client:client updatable)
  ((main :initarg :main :accessor main)
   (server-name :initform NIL :accessor server-name)
   (send-thread :initform NIL :accessor send-thread)))

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
  (setf (channel (main client)) NIL))

(defmethod find-channel (name (client client))
  (find-channel name (main client)))

(defmethod (setf find-channel) (value name (client client))
  (setf (find-channel name (main client)) value))

(defmethod handle-send-connection ((client client))
  (loop while (ignore-errors (open-stream-p (lichat-tcp-client::socket-stream client)))
        do (unless (process-updates client)
             (sleep 0.01))))

(defmethod update ((client client) (update lichat-protocol:update))
  (send update client))

;; FIXME: Queue for awaiting events from the GUI
(defmethod process ((update lichat-protocol:connect) (client client))
  (setf (server-name client) (lichat-protocol:from update)))

;; Deliver to main thread for synchronised processing
(defmethod process ((update lichat-protocol:update) (client client))
  (enqueue-update update (main client)))

(defun qsend (client type &rest initargs)
  (enqueue-update
   (apply #'make-instance type
          :from (lichat-tcp-client:name client)
          initargs)
   (client client)))
