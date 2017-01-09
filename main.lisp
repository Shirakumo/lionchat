#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defvar *main* NIL)

(define-widget main (QMainWindow)
  ((client :initform NIL :accessor client)))

(defmethod initialize-instance :before ((main main) &key)
  (setf *main* main))

(defmethod find-channel (name (main main))
  (find-channel name (slot-value main 'channel-list)))

(defmethod (setf find-channel) (value name (main main))
  (setf (find-channel name (slot-value main 'channel-list)) value))

(define-finalizer (main teardown)
  (when (client main)
    (close-connection (client main))))

(define-subwidget (main channel-list)
    (make-instance 'channel-list :main main)
  (q+:add-dock-widget main (q+:qt.left-dock-widget-area) channel-list))

(define-subwidget (main chat-area)
    (make-instance 'chat-area :main main)
  (setf (q+:central-widget main) chat-area))

(define-menu (main File)
  (:item "Connect"
         (when (client main)
           (close-connection (client main)))
         (with-finalizing ((c (make-instance 'connect)))
           (when (q+:exec c)
             (let ((client (apply #'make-instance 'client :main main (settings c))))
               (handler-case (setf (client main) (open-connection client))
                 (error (err)
                   (q+:qmessagebox-warning main "Lionchat Error"
                                           (format NIL "Connection failed: ~a" err))))))))
  (:item "Disconnect"
         (when (client main)
           (close-connection (client main))
           (setf (client main) NIL)))
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
