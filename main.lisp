#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defvar *main* NIL)

;; FIXME: Shortcuts for everything

(define-widget main (QMainWindow updatable)
  ((clients :initform NIL :accessor clients)
   (awaiting :initform NIL :accessor awaiting)))

(defmethod initialize-instance :before ((main main) &key)
  (setf *main* main))

(defmethod channel ((main main))
  (channel (slot-value main 'chat-area)))

(defmethod (setf channel) ((channel channel) (main main))
  (setf (channel (slot-value main 'channel-list)) channel)
  (setf (channel (slot-value main 'chat-area)) channel)
  (setf (channel (slot-value main 'user-list)) channel))

(defmethod (setf channel) ((channel null) (main main))
  (setf (channel (slot-value main 'channel-list)) channel)
  (setf (channel (slot-value main 'chat-area)) channel)
  (setf (channel (slot-value main 'user-list)) channel))

(defmethod (setf channel) (channel (main main))
  (setf (channel (slot-value main 'channel-list)) channel))

(defmethod prev-channel ((main main))
  (prev-channel (slot-value main 'channel-list)))

(defmethod next-channel ((main main))
  (next-channel (slot-value main 'channel-list)))

(defmethod (setf clients) :after (clients (main main))
  (let ((menu (slot-value main 'clients-menu)))
    (q+:clear menu)
    (dolist (client clients)
      (q+:add-action menu (name client)))))

(defmethod find-client (name (main main))
  (find name (clients main) :test #'string= :key #'name))

(defmethod (setf find-client) (value name (main main))
  (setf-named (clients main) name value))

(defmethod enqueue-update :after (update (main main))
  (unless (qobject-deleted main)
    (signal! main (process-updates))))

(define-signal (main process-updates) ())

(define-slot (main process-updates) ()
  (declare (connected main (process-updates)))
  (process-updates main))

(defmethod update ((main main) (update remove-client))
  (when (and (channel main)
             (eql (client update) (client (channel main))))
    (setf (channel main) NIL))
  (setf (find-client (name (client update)) main) NIL))

(defmethod update ((main main) (update lichat-protocol:update))
  (update (slot-value main 'channel-list) update)
  (update (slot-value main 'chat-area) update)
  (update (slot-value main 'user-list) update)
  ;; Process awaits
  (setf (awaiting main) (loop for await in (awaiting main)
                              unless (funcall await update)
                              collect await)))

(defmethod update :after ((main main) (update lichat-protocol:message))
  (when (and (ubiquitous:value :behavior :notify)
             (not (q+:is-visible main)))
    (let ((user (lichat-protocol:from update)))
      (q+:show (make-instance 'qui:notification
                              :title (format NIL "~a | Lionchat" (lichat-protocol:channel update))
                              :message (format NIL "<span style=\"color:~a\">~a</span>: ~a"
                                               (object-color user) user (lichat-protocol:text update))
                              :timeout 3000)))))

(defmethod update :after ((main main) (update lichat-protocol:join))
  (when (string= (username (client update))
                 (lichat-protocol:from update))
    (setf (channel main) (find-channel (lichat-protocol:channel update) (client update)))))

(define-initializer (main setup)
  ;; First-time setup
  (unless (ubiquitous:value :setup)
    (show-settings main :tab "Connections")
    (setf (ubiquitous:value :setup) T))
  ;; Init
  (update-connect-menu main)
  (loop for connection being the hash-values of (ubiquitous:value :connections)
        when (gethash :auto connection)
        do (apply #'maybe-connect main (alexandria:hash-table-plist connection)))
  (let ((layout (layout-data)))
    (when layout (q+:restore-state main layout))))

(define-finalizer (main teardown)
  (dolist (client (clients main))
    (close-connection client))
  (setf (layout-data) (q+:save-state main)))

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

(define-subwidget (main clients-menu)
    (q+:make-qmenu "&Clients"))

(define-subwidget (main connect-menu)
    (q+:make-qmenu "&Connect"))

(define-override (main close-event) (ev)
  (cond ((ubiquitous:value :behavior :tray)
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

(define-slot (main pick-client) ((action "QAction*"))
  (declare (connected clients-menu (triggered "QAction*")))
  (let ((client (find-client (q+:text action) main)))
    (setf (channel main) (find-channel T client))))

(define-slot (main pick-connection) ((action "QAction*"))
  (declare (connected connect-menu (triggered "QAction*")))
  (let ((client (ubiquitous:value :connections (q+:text action))))
    (apply #'maybe-connect main (alexandria:hash-table-plist client))))

(defun maybe-connect (main &rest args)
  (remf args :auto)
  (handler-case
      (let ((client (apply #'make-instance 'client :main main args)))
        (if (find-client (name client) main)
            (error "A connection named ~s already exists." (name client))
            (setf (find-client (name client) main)
                  (open-connection client))))
    (error (err)
      (show-error main "Connection to ~a failed:~%~a" (getf args :name) err))))

(defun show-settings (main &rest initargs)
  (with-slots-bound (main main)
    (with-finalizing ((settings (apply #'make-instance 'settings initargs)))
      (when (= 1 (q+:exec settings))
        (save settings)
        (setf (channel main) (channel main))
        (setf (q+:visible tray) (ubiquitous:value :behavior :tray))
        (update-connect-menu main)))))

(defun update-connect-menu (main)
  (with-slots-bound (main main)
    (q+:clear connect-menu)
    (loop for k being the hash-keys of (ubiquitous:value :connections)
          when (stringp k)
          do (q+:add-action connect-menu k))))

(define-menu (main File)
  (:menu connect-menu)
  (:item ("&Disconnect" (ctrl d))
         (when (channel main)
           (close-connection (client (channel main)))))
  (:separator)
  (:item ("&Settings..." (ctrl s))
         (show-settings main))
  (:item ("&Quit" (ctrl q))
         (q+:close main)
         (q+:qcoreapplication-quit)))

(define-menu (main Window)
  (:menu clients-menu)
  (:separator)
  (:item "Channels"
         (setf (q+:visible channel-list) (not (q+:is-visible channel-list))))
  (:item "Users"
         (setf (q+:visible user-list) (not (q+:is-visible user-list)))))

(define-menu (main Help)
  (:item "&About"
         (with-finalizing ((b (q+:make-qmessagebox)))
           (setf (q+:window-title b) "About LionChat")
           (setf (q+:icon-pixmap b) (q+:pixmap (q+:window-icon *qapplication*) 64 64))
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
    (flet ((inner-start ()
             (setf *package* #.*package*)
             (setf (q+:window-icon *qapplication*)
                   (q+:make-qicon (uiop:native-namestring (data "logo.svg"))))
             (q+:qfontdatabase-add-application-font (uiop:native-namestring (data "noto-emoji.ttf")))
             (q+:qapplication-set-quit-on-last-window-closed NIL)
             (make-instance 'main)))
      (with-main-window (main #'inner-start)))))
