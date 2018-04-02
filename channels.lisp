#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(define-consumer-widget channels (QDockWidget dockable)
  ()
  (:default-initargs :title "Channels"))

(define-subwidget (channels scroll) (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroll) T)
  (setf (q+:vertical-scroll-bar-policy scroll) (q+:qt.scroll-bar-always-on))
  (setf (q+:horizontal-scroll-bar-policy scroll) (q+:qt.scroll-bar-always-off))
  (setf (q+:widget channels) scroll))

(define-subwidget (channels list) (make-instance 'qui:listing :draggable NIL)
  (setf (q+:widget scroll) list))

(maiden:define-handler (channels added buffer-added) (c ev buffer)
  ;; (qui:add-widget (make-instance 'channel-entry :buffer buffer) (slot-value c 'list))
  )

(maiden:define-handler (channels removed buffer-removed) (c ev buffer)
  )

(define-widget channel-entry (QWidget qui:listing-item)
  ())

(defmethod initialize-instance :after ((entry channel-entry) &key buffer)
  ;; (setf (icon (slot-value entry 'icon)) icon)
  (setf (q+:text (slot-value entry 'name)) (name buffer)))

(define-subwidget (channel-entry icon) (make-instance 'toolbar-button))

(define-subwidget (channel-entry name) (q+:make-qlabel))

(define-subwidget (channel-entry layout) (q+:make-qhboxlayout channel-entry)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout icon)
  (q+:add-widget layout name))
