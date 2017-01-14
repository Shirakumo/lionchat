#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-subwidget (main shortcut-next)
    (q+:make-qshortcut (q+:make-qkeysequence "Ctrl+n") main))

(define-subwidget (main shortcut-prev)
    (q+:make-qshortcut (q+:make-qkeysequence "Ctrl+p") main))

(define-slot (main next-channel) ()
  (declare (connected shortcut-next (activated)))
  (next-channel main))

(define-slot (main prev-channel) ()
  (declare (connected shortcut-prev (activated)))
  (prev-channel main))
