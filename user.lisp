#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defclass user ()
  ((name :initarg :name :accessor name)
   (standing :initarg :standing :accessor standing))
  (:default-initargs
   :name (error "NAME required.")
   :standing :neutral))

(defmethod qui:coerce-item ((user user) container)
  (make-instance 'user-item :item user :container container))

(define-widget user-item (QWidget qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-subwidget (user-item name)
    (q+:make-qlabel (name (qui:widget-item user-item))))

(define-subwidget (user-item type)
    (q+:make-qlabel (case (standing (qui:widget-item user-item))
                      (:neutral "")
                      (:muted "_")
                      (:friend "@")))
  (setf (q+:fixed-width type) 20))

(define-subwidget (user-item layout) (q+:make-qhboxlayout user-item)
  (setf (q+:margin layout) 5)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout name)
  (q+:add-widget layout type))

(define-subwidget (user-item menu) (q+:make-qmenu)
  (setf (q+:context-menu-policy user-item) (q+:qt.custom-context-menu))
  (q+:add-action menu "Information")
  (q+:add-action menu "Kick")
  (q+:add-action menu "Contact")
  (q+:add-action menu "Un/Friend"))

(define-slot (user-item show-menu) ((pos "const QPoint&"))
  (declare (connected user-item (custom-context-menu-requested "const QPoint&")))
  (with-finalizing ((pos (q+:map-to-global user-item pos)))
    (let ((selected (q+:exec menu pos))
          (user (qui:widget-item user-item)))
      (cond ((null-qobject-p selected))
            ((string= "Information" (q+:text selected))
             (qsend (client *main*) 'lichat-protocol:user-info
                    :target (name user)))
            ((string= "Contact" (q+:text selected))
             (qsend (client *main*) 'lichat-protocol:create
                    :channel NIL)
             ;; With response for join
             (qsend (client *main*) 'lichat-protocol:pull
                    :channel NIL :target (name user)))
            ((string= "Kick" (q+:text selected))
             (qsend (client *main*) 'lichat-protocol:kick
                    :channel (active-channel *main*) :target (name user)))
            ((string= "Un/Friend" (q+:text selected))
             )))))
