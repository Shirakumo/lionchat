#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(defclass buffer ()
  ((name :initarg :name :accessor name)
   (items :initform (make-array 0 :adjustable T :fill-pointer T) :reader items))
  (:default-initargs :name (error "NAME required.")))

(defmethod initialize-instance :after ((buffer buffer) &key items)
  (let ((len (length items)))
    (adjust-array (items buffer) len :fill-pointer len)
    (replace (items buffer) items)))

(defgeneric item->widget (item))
(defgeneric data->widget (item data))

(defclass channel (buffer)
  ())

(defclass channel-item ()
  ((clock :initarg :clock :accessor clock)
   (user :initarg :user :accessor user)
   (data :initarg :data :accessor data))
  (:default-initargs
   :clock (get-universal-time)
   :user "System"
   :data (error "DATA required")))

(defun get-font ()
  (let ((font (q+:make-qfont "Monospace")))
    (setf (q+:style-hint font) (q+:qfont.type-writer))
    font))

(defmethod item->widget ((item channel-item))
  (let* ((widget (q+:make-qwidget))
         (layout (q+:make-qhboxlayout widget)))
    (setf (q+:margin layout) 0)
    (setf (q+:spacing layout) 5)
    (q+:add-widget layout (make-fixed-label (format-time (clock item) :time)))
    (q+:add-widget layout (make-fixed-label (user item) :length 10 :color (object-color (user item))))
    (q+:add-widget layout (data->widget item (data item)))
    widget))

(defun make-fixed-label (text &key (length (length text)) color)
  (let ((label (q+:make-qlabel text)))
    (setf (q+:alignment label) (q+:qt.align-top))
    (setf (q+:font label) (get-font))
    (setf (q+:fixed-width label) (round (* length 10)))
    (when color
      (let ((palette (q+:palette label)))
        (setf (q+:color palette (q+:foreground-role label)) color)
        (setf (q+:palette label) palette)))
    label))

(defun format-time (time &optional (format :full))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time time 0)
    (ecase format
      (:full (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s))
      (:date (format NIL "~4,'0d.~2,'0d.~2,'0d" yy mm dd))
      (:time (format NIL "~2,'0d:~2,'0d:~2,'0d" h m s))
      (:hour (format NIL "~2,'0d:~2,'0d" h m)))))

(defun object-color (object)
  (let* ((hash (sxhash object))
         (r (1- (* 16 (1+ (ash (logand hash #xF00) 8)))))
         (g (1- (* 16 (1+ (ash (logand hash #x0F0) 4)))))
         (b (1- (* 16 (1+ (ash (logand hash #x00F) 0))))))
    (q+:make-qcolor (min 200 (max 50 r)) (min 200 (max 50 g)) (min 200 (max 50 b)))))

(defclass message (channel-item)
  ())

(defmethod data->widget ((message message) text)
  (make-instance 'text-output :text text))

(define-widget text-output (QLabel)
  ())

(defmethod initialize-instance :after ((edit text-output) &key text)
  (setf (q+:font edit) (get-font))
  (setf (q+:word-wrap edit) T)
  (setf (q+:text-interaction-flags edit) (q+:qt.links-accessible-by-mouse))
  (setf (q+:size-policy edit) (values (q+:qsizepolicy.preferred) (q+:qsizepolicy.minimum)))
  (setf (q+:text edit) text))
