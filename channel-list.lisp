#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget channel-list (QDockWidget)
  ())

(define-initializer (channel-list setup)
  (setf (q+:features channel-list) (q+:qdockwidget.no-dock-widget-features))
  (setf (q+:title-bar-widget channel-list) (q+:make-qwidget channel-list)))

(define-subwidget (channel-list list)
    (q+:make-qlistwidget))

(define-subwidget (channel-list scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:widget scroller) list))

(define-subwidget (channel-list add)
    (q+:make-qpushbutton "+"))

(define-subwidget (channel-list channelname)
    (q+:make-qlineedit)
  (setf (q+:placeholder-text channelname) "Channel name..."))

(define-subwidget (channel-list center)
    (q+:make-qwidget)
  (setf (q+:widget channel-list) center)
  (setf (q+:size-policy center) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum))))

(define-subwidget (channel-list layout)
    (q+:make-qgridlayout center)
  (q+:add-widget layout scroller 0 0 1 2)
  (q+:add-widget layout channelname 1 0 1 1)
  (q+:add-widget layout add 1 1 1 1))
