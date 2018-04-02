#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(define-widget toolbar-button (QPushButton)
  ((action :initarg :action :accessor action))
  (:default-initargs :action NIL))

(defmethod initialize-instance :after ((button toolbar-button) &key icon solid)
  (setf (q+:flat button) T)
  (setf (q+:fixed-size button) (values 20 20))
  (setf (q+:size-policy button) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.maximum)))
  (setf (icon button) icon)
  (setf (solid button) solid))

(defmethod (setf icon) (icon (button toolbar-button))
  (setf (q+:text button) (etypecase icon
                           (null (name->icon :question))
                           (keyword (name->icon icon))
                           (character (string icon))
                           (string icon))))

(defmethod (setf solid) (solid (button toolbar-button))
  (setf (q+:style-sheet button) (format NIL "font-family: Font Awesome 5 Free; font-size: 16px; font-weight: ~:[400~;900~];" solid)))

(define-widget toolbar (QWidget)
  ())

(define-subwidget (toolbar layout) (q+:make-qhboxlayout toolbar)
  (setf (q+:margin layout) 0)
  (q+:add-stretch layout))

(define-signal (toolbar clicked) (string))

(define-slot (toolbar clicked) ()
  (signal! toolbar (clicked string) (symbol-name (action (q+:sender toolbar)))))

(defmethod add-button ((toolbar toolbar) action icon &key solid tooltip)
  (let ((button (make-instance 'toolbar-button :icon icon :solid solid :action action))
        (layout (slot-value toolbar 'layout)))
    (when tooltip (setf (q+:tool-tip button) tooltip))
    (q+:insert-widget layout (1- (q+:count layout)) button)
    (connect! button (clicked) toolbar (clicked))))
