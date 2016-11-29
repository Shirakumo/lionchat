#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget chat-area (QWidget)
  ())

(define-subwidget (chat-area output)
    (make-instance 'chat-output))

(define-subwidget (chat-area input)
    (make-instance 'chat-input))

(define-subwidget (chat-area send)
    (q+:make-qpushbutton "Send"))

(define-subwidget (chat-area layout)
    (q+:make-qgridlayout chat-area)
  (q+:add-widget layout output 0 0 1 3)
  (q+:add-widget layout input 1 0 1 2)
  (q+:add-widget layout send 1 2 1 1))

(define-widget chat-output (QTextBrowser)
  ())

(define-widget chat-input (QPlainTextEdit)
  ())
