#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defun known-users (client)
  (loop for name being the hash-keys of
        (ubiquitous:defaulted-value (make-hash-table :test 'equal) :users (name client))
        collect name))

(defclass user ()
  ((name :initarg :name :accessor name)
   (client :initarg :client :accessor client)))

(defmethod label ((user user))
  (cond ((muted-p user)
         "🔇")
        ((string= (name user) (server-name (client user)))
         "⚑")
        ((string= (name user) (username (client user)))
         "👤")
        (T
         "")))

(defmethod initialize-instance :before ((user user) &key name client)
  (unless name (error "NAME required."))
  (unless client (error "CLIENT required.")))

(defmethod muted-p ((user user))
  (ubiquitous:value :users (name (client user)) (name user) :muted))

(defmethod (setf muted-p) (value (user user))
  (setf (ubiquitous:value :users (name (client user)) (name user) :muted) value))

(defmethod qui:coerce-item ((user user) container)
  (make-instance 'user-item :item user :container container))

(define-widget user-item (QWidget qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-subwidget (user-item name) (q+:make-qlabel)
  (let ((user (qui:widget-item user-item)))
    (setf (q+:text name) (name user))
    (setf (q+:tool-tip name) (format NIL "~a // ~a" (name (client user)) (name user)))
    (setf (q+:style-sheet name) (format NIL "color:~a;" (object-color (name user))))))

(define-subwidget (user-item type)
    (q+:make-qlabel (label (qui:widget-item user-item)))
  (setf (q+:font type) (q+:make-qfont "NotoEmoji"))
  (setf (q+:fixed-width type) 20)
  (setf (q+:alignment type) (q+:qt.align-center)))

(define-subwidget (user-item layout) (q+:make-qhboxlayout user-item)
  (setf (q+:margin layout) 5)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout name)
  (q+:add-widget layout type))

(define-subwidget (user-item menu) (q+:make-qmenu)
  (setf (q+:context-menu-policy user-item) (q+:qt.custom-context-menu))
  (q+:add-action menu "Information")
  (q+:add-action menu "Contact")
  (q+:add-action menu "Un/Mute")
  (q+:add-action menu "Kick"))

(define-slot (user-item show-menu) ((pos "const QPoint&"))
  (declare (connected user-item (custom-context-menu-requested "const QPoint&")))
  (with-finalizing ((pos (q+:map-to-global user-item pos)))
    (let ((selected (q+:exec menu pos))
          (user (qui:widget-item user-item)))
      (cond ((null-qobject-p selected))
            ((string= "Information" (q+:text selected))
             (qsend user 'lichat-protocol:user-info
                    :target (name user)))
            ((string= "Kick" (q+:text selected))
             (qsend user 'lichat-protocol:kick
                    :channel (name (channel *main*)) :target (name user)))
            ((string= "Un/Mute" (q+:text selected))
             (setf (muted-p user) (not (muted-p user)))
             (setf (q+:text type) (label user)))
            ((string= "Contact" (q+:text selected))
             (let ((id (lichat-protocol:id (qsend user 'lichat-protocol:create :channel NIL))))
               (with-awaiting (update id *main*)
                 (typecase update
                   (lichat-protocol:join
                    (qsend user 'lichat-protocol:pull
                           :channel (lichat-protocol:channel update) :target (name user)))
                   (lichat-protocol:update-failure
                    (show-error user-item "Failed to open personal connection."))))))))))
