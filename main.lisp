#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)

(define-widget main (QMainWindow)
  ())

(define-subwidget (main channel-list)
    (make-instance 'channel-list)
  (q+:add-dock-widget main channel-list (q+:qmainwindow.left-dock-widget-area)))

(define-subwidget (main chat-area)
    (make-instance 'chat-area)
  (setf (q+:central-widget main) chat-area))

(define-menu (main File)
    (:item "Connect"
           (with-finalizing ((c (make-instance 'connect)))
             (when (q+:exec c)
               (apply #'connect (settings c)))))
  (:item "Disconnect"
         (disconnect (connection chat-area)))
  (:separator)
  (:item "Quit"
         (q+:close main)))

(define-menu (main Help)
  (:item "About"
         (with-finalizing ((b (q+:make-qmessagebox)))
           (setf (q+:window-title b) "About LionChat")
           (setf (q+:text b) (system-about))
           (q+:exec b))))

(defun system-about ()
  (let ((system (asdf:find-system :lionchat)))
    (format NIL "~a<br />
The source code is openly available and licensed under the ~a license.<br />
<br />
Homepage: <a href=\"~a~:*\">~a</a><br />
Author: ~a<br />
Version: ~a"
            (asdf:system-description system)
            (asdf:system-license system)
            (asdf:system-homepage system)
            (asdf:system-author system)
            (asdf:component-version system))))

(defun start (&key)
  (let ((*package* #.*package*))
    (v:output-here)
    (with-main-window (main 'main :name "Lionchat"))))
