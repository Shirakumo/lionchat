#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defclass channel ()
  ((name :initarg :name :accessor name)
   (updates :initform (make-array 0 :adjustable T :fill-pointer T) :accessor updates)
   (users :initform () :accessor users)))

(defmethod update ((channel channel) (update lichat-protocol:update))
  (vector-push-extend update (updates channel)))

(defmethod update ((channel channel) (update lichat-protocol:users))
  (setf (users channel) (lichat-protocol:users update)))

(defmethod update :after ((channel channel) (update lichat-protocol:update))
  (when (eql channel (channel (slot-value *main* 'chat-area)))
    (update (slot-value *main* 'chat-area) update)))
