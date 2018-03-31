#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(define-widget input (QWidget)
  ())

(define-initializer (input setup)
  (setf (q+:minimum-height input) 50)
  (setf (q+:size-policy input) (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.maximum))))

(define-subwidget (input toolbar) (make-instance 'toolbar)
  (add-button toolbar :emote :smile :tooltip "Insert an emote.")
  (add-button toolbar :attach :paperclip :tooltip "Attach a file."))

(define-subwidget (input textbox) (make-instance 'textbox))

(define-subwidget (input layout) (q+:make-qvboxlayout input)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout toolbar)
  (q+:add-widget layout textbox))

(define-widget textbox (QPlainTextEdit)
  ())

(define-signal (textbox send) (string))

(define-override (textbox key-press-event) (ev)
  (qtenumcase (q+:key ev)
    ((q+:qt.key_return)
     (cond ((< 0 (logand (q+:qt.control-modifier) (q+:modifiers ev)))
            (q+:insert-plain-text textbox (string #\Linefeed)))
           (T
            (setf (q+:accepted ev) T)
            (signal! textbox (send string) (q+:to-plain-text textbox))
            (setf (q+:plain-text textbox) ""))))
    (T
     (stop-overriding))))
