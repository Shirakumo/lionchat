#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(define-widget chat (QMainWindow maiden:core)
  ())

(define-initializer (chat setup)
  ;; FIXME
  (dolist (font (directory (make-pathname :name :wild :type "ttf" :defaults (asdf:system-relative-pathname :lionchat "data/"))))
    (q+:qfontdatabase-add-application-font (uiop:native-namestring font)))
  (setf (q+:dock-options chat) (logior (q+:qmainwindow.allow-tabbed-docks)
                                       (q+:qmainwindow.allow-nested-docks))))

(define-finalizer (chat teardown)
  (q+:qfontdatabase-remove-all-application-fonts))

(define-subwidget (chat output) (make-instance 'output)
  (setf (buffer output) (make-instance 'buffer :items (list (make-instance 'message :data "This is a really long line with a lot of text to test the word wrapping and such things.")
                                                            (make-instance 'message :data "Hi")))))

(define-subwidget (chat input) (make-instance 'input))

(define-subwidget (chat center) (q+:make-qsplitter (q+:qt.vertical))
  (setf (q+:central-widget chat) center)
  (q+:add-widget center output)
  (q+:add-widget center input)
  (setf (q+:children-collapsible center) NIL)
  (setf (q+:stretch-factor center 0) 1)
  (setf (q+:stretch-factor center 1) 0)
  (setf (q+:sizes center) (list 1 100)))

(define-subwidget (chat channels) (make-instance 'channels)
  (q+:add-dock-widget chat (q+:qt.right-dock-widget-area) channels))

(define-subwidget (chat users) (make-instance 'users)
  (q+:add-dock-widget chat (q+:qt.right-dock-widget-area) users))

(define-menu (chat file)
  (:item "Quit" (q+:close chat)))

(define-menu (chat view)
  (:item "Channels"
         (setf (q+:visible channels) (not (q+:is-visible channels))))
  (:item "Users"
         (setf (q+:visible users) (not (q+:is-visible users)))))

(defun start ()
  (with-main-window (chat 'chat)))


