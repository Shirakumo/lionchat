#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defvar *main* NIL)

(define-widget main (QMainWindow updatable)
  ((client :initform NIL :accessor client)))

(defmethod initialize-instance :before ((main main) &key)
  (setf *main* main))

(defmethod find-channel (name (main main))
  (find-channel name (slot-value main 'channel-list)))

(defmethod (setf find-channel) (value name (main main))
  (setf (find-channel name (slot-value main 'channel-list)) value))

(defmethod channel ((main main))
  (channel (slot-value main 'chat-area)))

(defmethod (setf channel) (channel (main main))
  (setf (channel (slot-value main 'channel-list)) channel)
  (setf (channel (slot-value main 'chat-area)) channel)
  (setf (channel (slot-value main 'user-list)) channel))

(defmethod find-user (name (main main))
  (find-user name (slot-value main 'user-list)))

(defmethod (setf find-user) (value name (main main))
  (setf (find-user name (slot-value main 'user-list)) value))

(defmethod enqueue-update :after (update (main main))
  (signal! main (process-updates)))

(define-signal (main process-updates) ())

(define-slot (main process-updates) ()
  (declare (connected main (process-updates)))
  (process-updates main))

(defmethod update ((main main) (update lichat-protocol:update))
  (update (channel main) update))

(defmethod update ((main main) (update lichat-protocol:channel-update))
  (update (slot-value main 'user-list) update)
  (update (find-channel (lichat-protocol:channel update) main) update))

(defmethod update ((main main) (update lichat-protocol:join))
  (when (string= (name (client main)) (lichat-protocol:from update))
    (setf (find-channel (lichat-protocol:channel update) main)
          (make-instance 'channel :name (lichat-protocol:channel update)
                                  :primary-p (equal (server-name (client main))
                                                    (lichat-protocol:channel update))))
    ;; Get user listing for the new channel.
    (qsend (client main) 'lichat-protocol:users :channel (lichat-protocol:channel update)))
  (let ((channel (find-channel (lichat-protocol:channel update) main)))
    (update channel update)
    (setf (channel main) channel)))

(defmethod update ((main main) (update lichat-protocol:leave))
  (update (find-channel (lichat-protocol:channel update) main)
          update)
  (when (string= (name (client main)) (lichat-protocol:from update))
    (setf (find-channel (lichat-protocol:channel update) main)
          NIL)))

(define-finalizer (main teardown)
  (when (client main)
    (close-connection (client main))))

(define-subwidget (main channel-list)
    (make-instance 'channel-list :main main)
  (q+:add-dock-widget main (q+:qt.left-dock-widget-area) channel-list))

(define-subwidget (main user-list)
    (make-instance 'user-list :main main)
  (q+:add-dock-widget main (q+:qt.right-dock-widget-area) user-list))

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
