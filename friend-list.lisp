#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget friend-list (QDockWidget)
  ((main :initarg :main :accessor main)))

(define-initializer (friend-list setup)
  (setf (q+:features friend-list) (q+:qdockwidget.dock-widget-movable))
  (setf (q+:window-title friend-list) "Friends"))

(define-subwidget (friend-list list)
    (make-instance 'qui:listing :draggable NIL
                                :sorting (lambda (a b)
                                           (string< (name a) (name b)))))
