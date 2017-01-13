#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defclass channel ()
  ((primary-p :initarg :primary-p :accessor primary-p)
   (name :initarg :name :accessor name)
   (client :initarg :client :accessor client)
   (updates :initform (make-array 0 :adjustable T :fill-pointer T) :accessor updates)
   (users :initform () :accessor users))
  (:default-initargs
   :primary-p NIL))

(defmethod initialize-instance :before ((channel channel) &key name client)
  (unless name (error "NAME required."))
  (unless client (error "CLIENT required.")))

(defmethod initialize-instance :after ((channel channel) &key client name)
  (when (equal (server-name client) name)
    (setf (primary-p channel) T)))

(defmethod anonymous-p ((channel channel))
  (char= #\@ (char (name channel) 0)))

(defmethod process ((update lichat-protocol:update) (channel channel))
  (vector-push-extend update (updates channel)))

;; Maintain userlist
(defmethod process ((update lichat-protocol:users) (channel channel))
  (setf (users channel) (loop for user in (lichat-protocol:users update)
                              collect (or (find-user user (client channel))
                                          (error "INCONSISTENCY: ~a not found" user)))))

(defmethod process :after ((update lichat-protocol:join) (channel channel))
  (pushnew (or (find-user (lichat-protocol:from update) (client channel))
               (error "INCONSISTENCY: ~a not found" (lichat-protocol:from update)))
           (users channel)))

(defmethod process :after ((update lichat-protocol:leave) (channel channel))
  (setf (users channel) (remove (lichat-protocol:from update) (users channel)
                                :key #'name :test #'string=)))

(defmethod qui:coerce-item ((channel channel) container)
  (make-instance 'channel-item :item channel :container container))

(define-widget channel-item (QWidget qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(defmethod qui:drag-start ((channel-item channel-item) x y)
  (declare (ignore x y))
  (setf (channel *main*) (qui:widget-item channel-item)))

(define-subwidget (channel-item name) (q+:make-qlabel)
  (let ((channel (qui:widget-item channel-item)))
    (setf (q+:text name) (name channel))
    (setf (q+:tool-tip name) (format NIL "~a/~a" (name channel) (name (client channel))))))

(define-subwidget (channel-item type)
    (q+:make-qlabel (cond ((primary-p (qui:widget-item channel-item))
                           "#")
                          ((anonymous-p (qui:widget-item channel-item))
                           "@")
                          (T "")))
  (setf (q+:fixed-width type) 20))

(define-subwidget (channel-item layout) (q+:make-qhboxlayout channel-item)
  (setf (q+:margin layout) 5)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout name)
  (q+:add-widget layout type))

(define-subwidget (channel-item menu) (q+:make-qmenu)
  (setf (q+:context-menu-policy channel-item) (q+:qt.custom-context-menu))
  (q+:add-action menu "Leave")
  (q+:add-action menu "Settings")
  (q+:add-action menu "Un/Favourite"))

(define-slot (channel-item show-menu) ((pos "const QPoint&"))
  (declare (connected channel-item (custom-context-menu-requested "const QPoint&")))
  (with-finalizing ((pos (q+:map-to-global channel-item pos)))
    (let ((selected (q+:exec menu pos))
          (channel (qui:widget-item channel-item)))
      (cond ((null-qobject-p selected))
            ((string= "Leave" (q+:text selected))
             (qsend channel 'lichat-protocol:leave
                    :channel (name channel)))
            ((string= "Settings" (q+:text selected))
             (with-finalizing ((settings (make-instance 'channel-settings :channel channel)))
               (q+:exec settings)))
            ((string= "Un/Favourite" (q+:text selected))
             )))))

(define-widget channel-settings (QDialog)
  ((channel :initarg :channel :accessor channel)))

(defmethod initialize-instance :after ((channel-settings channel-settings) &key channel)
  (setf (q+:text (slot-value channel-settings 'name))
        (name channel))
  (let ((id (lichat-protocol:id (qsend channel 'lichat-protocol:permissions
                                       :channel (name channel)))))
    (with-awaiting (update id (main (client channel)))
      (cond ((typep update 'lichat-protocol:permissions)
             (setf (q+:text (slot-value channel-settings 'permissions))
                   (permissions-string (lichat-protocol:permissions update)))
             (setf (q+:read-only (slot-value channel-settings 'permissions)) NIL))
            (T
             (show-error channel-settings "Failed to retrieve permissions."))))))

(define-subwidget (channel-settings name)
    (q+:make-qlineedit)
  (setf (q+:read-only name) T))

(define-subwidget (channel-settings permissions)
    (q+:make-qtextedit)
  (setf (q+:text permissions) "Loading...")
  (setf (q+:read-only permissions) T))

(define-subwidget (channel-settings font) (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font permissions) font))

(define-subwidget (channel-settings save)
    (q+:make-qpushbutton "Save"))

(define-subwidget (channel-settings layout)
    (q+:make-qvboxlayout channel-settings)
  (let ((form (q+:make-qformlayout)))
    (q+:add-row form "Name" name)
    (q+:add-row form "Permissions" permissions)
    (q+:add-layout layout form))
  (q+:add-widget layout save))

(define-slot (channel-settings save) ()
  (declare (connected save (clicked)))
  (let ((id (lichat-protocol:id (qsend channel 'lichat-protocol:permissions
                                       :channel (name channel)
                                       :permissions (read-permissions (q+:to-plain-text permissions))))))
    (with-awaiting (update id (main (client channel)))
      (cond ((typep update 'lichat-protocol:permissions)
             (q+:accept channel-settings))
            ((typep update 'lichat-protocol:update-failure)
             (show-error channel-settings "Failed to update permissions:~%~a"
                         (lichat-protocol:text update)))))))
