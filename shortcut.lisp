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

(define-subwidget (main shortcut-leave)
    (q+:make-qshortcut (q+:make-qkeysequence "Ctrl+l") main))

(macrolet ((define-n-shortcut (n)
             (let ((name (intern (format NIL "shortcut-~a" n))))
               `(progn (define-subwidget (main ,name)
                           (q+:make-qshortcut (q+:make-qkeysequence ,(format NIL "Alt+~a" n)) main))
                       (define-slot (main ,name) ()
                         (declare (connected ,name (activated)))
                         (setf (channel main) ,(mod (1- n) 10)))))))
  (define-n-shortcut 0)
  (define-n-shortcut 1)
  (define-n-shortcut 2)
  (define-n-shortcut 3)
  (define-n-shortcut 4)
  (define-n-shortcut 5)
  (define-n-shortcut 6)
  (define-n-shortcut 7)
  (define-n-shortcut 8)
  (define-n-shortcut 9))

(define-slot (main next-channel) ()
  (declare (connected shortcut-next (activated)))
  (next-channel main))

(define-slot (main prev-channel) ()
  (declare (connected shortcut-prev (activated)))
  (prev-channel main))

(define-slot (main leave-channel) ()
  (declare (connected shortcut-leave (activated)))
  (let ((channel (channel main)))
    (when channel
      (qsend channel 'lichat-protocol:leave :channel (name channel)))))
