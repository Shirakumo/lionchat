#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget repl (QDockWidget)
  ((main :initarg :main :accessor main)))

(define-initializer (repl setup)
  (setf (q+:features repl) (logior (q+:qdockwidget.dock-widget-movable)
                                   (q+:qdockwidget.dock-widget-floatable)))
  (setf (q+:window-title repl) "REPL")
  (setf (q+:object-name repl) "repl"))

(define-subwidget (repl actual-repl)
    (let ((*package* #.*package*)) (make-instance 'qui:repl))
  (setf (q+:widget repl) actual-repl))
