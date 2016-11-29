#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)

(define-widget channel-list (QDockWidget)
  ())

(define-initializer (channel-list setup)
  (setf (q+:features channel-list) (q+:qdockwidget.no-dock-widget-features))
  (setf (q+:title-bar-widget channel-list) (q+:make-qwidget channel-list)))
