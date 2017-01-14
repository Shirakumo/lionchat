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

(defmethod (setf channels) :after (value (channel-list channel-list))
  (update-listing channel-list))

(defmethod update ((channel-list channel-list) (update lichat-protocol:join))
  (when (string= (lichat-protocol:from update)
                 (username (client update)))
    (setf (channels channel-list)
          (append (channels (client update))
                  (remove (client update) (channels channel-list) :key #'client)))))

(defmethod update ((channel-list channel-list) (update lichat-protocol:leave))
  (when (string= (lichat-protocol:from update)
                 (username (client update)))
    (setf (channels channel-list)
          (append (channels (client update))
                  (remove (client update) (channels channel-list) :key #'client)))))

(define-initializer (channel-list setup)
  (setf (q+:features channel-list) (q+:qdockwidget.dock-widget-movable))
  (setf (q+:window-title channel-list) "Channels")
  (setf (q+:object-name channel-list) "channel-list"))

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
  (setf (q+:size-policy center) (values (q+:qsizepolicy.preferred)
                                        (q+:qsizepolicy.preferred))))

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
    (when (and (string/= "" name)
               (channel channel-list))
      (qsend (channel channel-list)
             'lichat-protocol:join
             :channel name)
      (setf (q+:text channelname) ""))))

(define-slot (channel-list create) ()
  (declare (connected create (clicked)))
  (let ((name (q+:text channelname)))
    (when (and (string/= "" name)
               (channel channel-list))
      (qsend (channel channel-list)
             'lichat-protocol:create
             :channel name)
      (setf (q+:text channelname) ""))))

(defmethod update-listing ((channel-list channel-list))
  (let ((list (slot-value channel-list 'list)))
    (qui:clear-layout list T)
    (dolist (channel (channels channel-list))
      (qui:add-item channel list))))

(defmethod channel ((channel-list channel-list))
  (qui:active-item (slot-value channel-list 'list)))

(defmethod (setf channel) ((channel null) (channel-list channel-list))
  (setf (channels channel-list) NIL))

(defmethod (setf channel) ((channel channel) (channel-list channel-list))
  (setf (qui:active-item (slot-value channel-list 'list)) channel))

(defmethod (setf channel) ((channel integer) (channel-list channel-list))
  (setf (channel (main channel-list))
        (find-channel channel channel-list)))

(defmethod (setf channel) ((channel string) (channel-list channel-list))
  (setf (channel (main channel-list))
        (find-channel channel channel-list)))

(defmethod find-channel ((name string) (channel-list channel-list))
  (qui:find-item name (slot-value channel-list 'list) :key #'name :test #'string-equal))

(defmethod find-channel ((index integer) (channel-list channel-list))
  (qui:item-at index (slot-value channel-list 'list)))

(defmethod next-channel ((channel-list channel-list))
  (let ((pos (qui:item-position (channel channel-list) (slot-value channel-list 'list))))
    (setf (channel channel-list)
          (mod (1+ pos) (length (channels channel-list))))))

(defmethod prev-channel ((channel-list channel-list))
  (let ((pos (qui:item-position (channel channel-list) (slot-value channel-list 'list))))
    (setf (channel channel-list)
          (mod (1- pos) (length (channels channel-list))))))
