#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defvar *main* NIL)

(define-widget main (QMainWindow updatable)
  ((clients :initform NIL :accessor clients)
   (exiting :initform NIL :accessor exiting)))

(defmethod initialize-instance :before ((main main) &key)
  (setf *main* main))

(defmethod channel ((main main))
  (channel (slot-value main 'chat-area)))

(defmethod (setf channel) (channel (main main))
  (setf (channel (slot-value main 'channel-list)) channel)
  (setf (channel (slot-value main 'chat-area)) channel)
  (setf (channel (slot-value main 'user-list)) channel))

(defmethod find-client (name (main main))
  (find name (clients main) :test #'string= :key #'name))

(defmethod (setf find-client) (value name (main main))
  (setf-named (clients main) name value))

(defmethod enqueue-update :after (update (main main))
  (signal! main (process-updates)))

(define-signal (main process-updates) ())

(define-slot (main process-updates) ()
  (declare (connected main (process-updates)))
  (process-updates main))

(defmethod update ((main main) (update lichat-protocol:update))
  ;; FIXME: Queue for awaiting events from the GUI
  (update (slot-value main 'channel-list) update)
  (update (slot-value main 'chat-area) update)
  (update (slot-value main 'user-list) update))

(defmethod update :after ((main main) (update lichat-protocol:join))
  (when (string= (username (client update))
                 (lichat-protocol:from update))
    (setf (channel main) (find-channel (lichat-protocol:channel update) (client update)))))

(define-finalizer (main teardown)
  (dolist (client (clients main))
    (close-connection client)))

(define-subwidget (main channel-list)
    (make-instance 'channel-list :main main)
  (q+:add-dock-widget main (q+:qt.left-dock-widget-area) channel-list))

(define-subwidget (main user-list)
    (make-instance 'user-list :main main)
  (q+:add-dock-widget main (q+:qt.right-dock-widget-area) user-list))

(define-subwidget (main chat-area)
    (make-instance 'chat-area :main main)
  (setf (q+:central-widget main) chat-area))

(define-subwidget (main tray)
    (make-instance 'tray :main main)
  (when (ubiquitous:value :behavior :tray)
    (q+:show tray)))

(define-override (main close-event) (ev)
  (cond ((and (not (exiting main)) (ubiquitous:value :behavior :tray))
         (q+:hide main)
         (q+:ignore ev))
        (T
         (q+:accept ev))))

(define-override (main change-event) (ev)
  (cond ((ubiquitous:value :behavior :tray)
         (when (and (enum-equal (q+:qevent.window-state-change) (q+:type ev))
                    (q+:is-minimized main))
           (q+:hide main))
         (q+:ignore ev))
        (T
         (q+:accept ev))))

(define-menu (main File)
  (:item "Connect..."
         (with-finalizing ((c (make-instance 'connect)))
           (when (= 1 (q+:exec c))
             (handler-case
                 (let ((client (apply #'make-instance 'client :main main (settings c))))
                   (if (find-client (name client) main)
                       (error "A connection named ~s already exists." (name client))
                       (setf (find-client (name client) main)
                             (open-connection client))))
               (error (err)
                 (q+:qmessagebox-warning main "Lionchat Error"
                                         (escape-html
                                          (format NIL "Connection failed:~%~a" err))))))))
  (:item "Disconnect"
         (when (channel main)
           (close-connection (client (channel main)))))
  (:separator)
  (:item "Settings..."
         (with-finalizing ((c (make-instance 'settings)))
           (when (= 1 (q+:exec c))
             (setf (ubiquitous:value) (settings c))
             (setf (channel main) (channel main))
             (setf (q+:visible tray) (ubiquitous:value :behavior :tray)))))
  (:item "Quit"
         (setf (exiting main) T)
         (q+:close main)))

(define-menu (main Window)
  (:item "Channels"
         (setf (q+:visible channel-list) (not (q+:is-visible channel-list))))
  (:item "Users"
         (setf (q+:visible user-list) (not (q+:is-visible user-list)))))

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
    (default-configuration)
    (with-main-window (main 'main :name "Lionchat"))))
