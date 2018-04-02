#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(maiden:define-event buffer-added (maiden:client-event)
  ((buffer :initarg :buffer :reader buffer)))

(maiden:define-event buffer-removed (maiden:client-event)
  ((buffer :initarg :buffer :reader buffer)))

(maiden:define-consumer controller (maiden:agent)
  ((chat :initarg :chat :reader chat)
   (buffers :initform (make-hash-table :test 'equal) :accessor buffers)))

(maiden:define-handler (controller join maiden-client-entities:user-entered) (c ev client channel user)
  (when (equal (maiden-client-entities:username client)
               (maiden-client-entities:username user))
    ;; FIXME: Ident system
    (let ((buffer (make-instance 'channel :name (format NIL "~a/~a" (maiden:name client) (maiden:name channel)))))
      (setf (gethash (name buffer) (buffers c)) buffer)
      (maiden:respond ev :class 'buffer-added :buffer buffer))))

(maiden:define-handler (controller leave maiden-client-entities:user-left) (c ev client channel user)
  (when (equal (maiden-client-entities:username client)
               (maiden-client-entities:username user))
    (let ((buffer (gethash (format NIL "~a/~a" (maiden:name client) (maiden:name channel)) (buffers c))))
      (remhash (name buffer) (buffers c))
      (maiden:respond ev :class 'buffer-removed :buffer buffer))))
