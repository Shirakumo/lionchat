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

(define-subwidget (friend-list scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:widget scroller) list))

(define-subwidget (friend-list center)
    (q+:make-qwidget)
  (setf (q+:widget friend-list) center)
  (setf (q+:size-policy center) (values (q+:qsizepolicy.preferred)
                                        (q+:qsizepolicy.preferred))))

(define-subwidget (friend-list layout)
    (q+:make-qgridlayout center)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 5)
  (q+:add-widget layout scroller 0 0 1 1))
