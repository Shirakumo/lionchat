#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget channel-list (QDockWidget)
  ((main :initarg :main :accessor main)
   (channels :initform () :accessor channels)))

(defmethod find-channel (name (channel-list channel-list))
  (if (eql name T)
      (find T (channels channel-list) :key #'primary)
      (find name (channels channel-list) :key #'name :test #'string-equal)))

(defmethod (setf find-channel) (value name (channel-list channel-list))
  (let ((prev (find-channel name channel-list))
        (channels (channels channel-list)))
    (unless (eql value prev)
      (when prev
        (finalize prev)
        (setf channels (remove prev channels)))
      (when value
        (setf channels (sort (list* value (channels channel-list))
                             #'string< :key #'name)))
      (setf (channels channel-list) channels)))
  value)

(defmethod (setf channels) :after (value (channel-list channel-list))
  (update-listing channel-list))

(define-initializer (channel-list setup)
  (setf (q+:features channel-list) (q+:qdockwidget.no-dock-widget-features))
  (setf (q+:title-bar-widget channel-list) (q+:make-qwidget channel-list)))

(define-subwidget (channel-list list)
    (make-instance 'qui:listing :draggable NIL
                                :sorting (lambda (a b)
                                           (or (primary-p a)
                                               (and (not (primary-p b))
                                                    (or (anonymous-p a)
                                                        (and (not (anonymous-p b))
                                                             (string< (name a) (name b)))))))))

(define-subwidget (channel-list scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:widget scroller) list))

(define-subwidget (channel-list join)
    (q+:make-qpushbutton "j")
  (setf (q+:fixed-width join) 20))

(define-subwidget (channel-list create)
    (q+:make-qpushbutton "c")
  (setf (q+:fixed-width create) 20))

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
  (q+:add-widget layout scroller 0 0 1 3)
  (q+:add-widget layout channelname 1 0 1 1)
  (q+:add-widget layout join 1 1 1 1)
  (q+:add-widget layout create 1 2 1 1))

(define-slot (channel-list join) ()
  (declare (connected join (clicked)))
  (let ((name (q+:text channelname)))
    (when (string/= "" name)
      (qsend (client (main channel-list))
             'lichat-protocol:join
             :channel name)
      (setf (q+:text channelname) ""))))

(define-slot (channel-list create) ()
  (declare (connected create (clicked)))
  (let ((name (q+:text channelname)))
    (when (string/= "" name)
      (qsend (client (main channel-list))
             'lichat-protocol:create
             :channel name)
      (setf (q+:text channelname) ""))))

(defmethod update-listing ((channel-list channel-list))
  (let ((list (slot-value channel-list 'list)))
    (qui:clear-layout list T)
    (dolist (channel (channels channel-list))
      (qui:add-item channel list))))

(defmethod active-channel ((channel-list channel-list))
  (qui:active-item (slot-value channel-list 'list)))

(defmethod (setf active-channel) ((channel channel) (channel-list channel-list))
  (setf (qui:active-item (slot-value channel-list 'list)) channel))
