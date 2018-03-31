#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(define-widget title (QWidget)
  ())

(defmethod initialize-instance :after ((widget title) &key title)
  (setf (title widget) title))

(define-subwidget (title label) (q+:make-qlabel))

(define-subwidget (title close) (make-instance 'toolbar-button :icon :times-circle :solid T))

(define-subwidget (title layout) (q+:make-qhboxlayout title)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout label)
  (q+:add-widget layout close))

(defmethod (setf title) (string (title title))
  (setf (q+:text (slot-value title 'label)) (or string "")))

(defmethod title ((title title))
  (q+:text (slot-value title 'label)))

(define-widget dockable (QDockWidget)
  ()
  (:default-initargs :title NIL))

(defmethod initialize-instance :after ((dockable dockable) &key title)
  (let ((title (make-instance 'title :title title)))
    (setf (q+:title-bar-widget dockable) title)
    (connect! (slot-value title 'close) (clicked) dockable (close))))

(define-initializer (dockable setup)
  (setf (q+:features dockable) (logior (q+:qdockwidget.dock-widget-closable)
                                       (q+:qdockwidget.dock-widget-movable)
                                       (q+:qdockwidget.dock-widget-floatable))))

(define-slot (dockable close) ()
  (q+:hide dockable))
