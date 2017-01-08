#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget channel-list (QDockWidget)
  ((main :initarg :main :accessor main)
   (channels :initform (make-hash-table :test 'equalp) :accessor channels)))

(defmethod find-channel (name (channel-list channel-list))
  (gethash name (channels channel-list)))

(defmethod (setf find-channel) (value name (channel-list channel-list))
  (if value
      (setf (gethash name (channels channel-list)) value)
      (remhash name (channels channel-list)))
  (update-listing channel-list)
  value)

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
  (setf (q+:size-policy center) (values (q+:qsizepolicy.maximum)
                                        (q+:qsizepolicy.minimum))))

(define-subwidget (channel-list layout)
    (q+:make-qgridlayout center)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 5)
  (q+:add-widget layout scroller 0 0 1 2)
  (q+:add-widget layout channelname 1 0 1 1)
  (q+:add-widget layout add 1 1 1 1))

(define-slot (channel-list join) ()
  (declare (connected add (clicked)))
  (let ((name (q+:text channelname)))
    (when (string/= "" name)
      (qsend (client (main channel-list))
             'lichat-protocol:create
             :channel name)
      (setf (q+:text channelname) ""))))

(define-slot (channel-list show) ((item "QListWidgetItem *"))
  (declare (connected list (item-clicked "QListWidgetItem *")))
  (let ((channel (find-channel (q+:text item) channel-list)))
    (setf (channel (slot-value (main channel-list) 'chat-area))
          channel)))

(defmethod update-listing ((channel-list channel-list))
  (let ((list (slot-value channel-list 'list)))
    (q+:clear list)
    (q+:add-items list (loop for name being the hash-keys of (channels channel-list)
                             collect name))))
