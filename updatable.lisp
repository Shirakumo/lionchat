#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defclass updatable ()
  ((queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor queue)
   (back-queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor back-queue)
   (lock :initform (bt:make-lock) :accessor lock)))

(defmethod update (target update))

(defmethod enqueue-update (update (updatable updatable))
  (bt:with-lock-held ((lock updatable))
    (vector-push-extend update (queue updatable))))

(defmethod process-updates ((updatable updatable))
  (let ((queue))
    (bt:with-lock-held ((lock updatable))
      (rotatef (queue updatable) (back-queue updatable))
      (setf queue (back-queue updatable)))
    (unless (= 0 (length queue))
      (loop for i = (decf (fill-pointer queue))
            for update = (shiftf (aref queue i) NIL)
            do (update updatable update)
            while (< 0 i))
      T)))
