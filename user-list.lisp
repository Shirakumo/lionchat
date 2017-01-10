#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget user-list (QDockWidget)
  ((main :initarg :main :accessor main)
   (channel :initform NIL :accessor channel)
   (users :initform NIL :accessor users)))

(defmethod find-user (name (user-list user-list))
  (find name (users user-list) :key #'name :test #'string-equal))

(defmethod (setf find-user) (value name (user-list user-list))
  (let ((prev (find-user name user-list))
        (users (users user-list)))
    (unless (eql value prev)
      (setf users (remove prev users))
      (when value
        (setf users (sort (list* value users)
                          #'string< :key #'name)))
      (setf (users user-list) users)))
  value)

(defmethod (setf users) :after (users (user-list user-list))
  (update-listing user-list))

(defmethod (setf channel) :after (channel (user-list user-list))
  (update-listing user-list))

(defmethod update ((user-list user-list) update))

(defmethod update ((user-list user-list) (update lichat-protocol:users))
  (when (primary-p (find-channel (lichat-protocol:channel update) (main user-list)))
    (dolist (name (lichat-protocol:users update))
      (unless (find-user name user-list)
        (setf (find-user name user-list)
              (make-instance 'user :name name))))))

(defmethod update ((user-list user-list) (update lichat-protocol:join))
  (when (primary-p (find-channel (lichat-protocol:channel update) (main user-list)))
    (setf (find-user (lichat-protocol:from update) user-list)
          (make-instance 'user :name (lichat-protocol:from update)))))

(defmethod update ((user-list user-list) (update lichat-protocol:leave))
  (when (primary-p (find-channel (lichat-protocol:channel update) (main user-list)))
    (setf (find-user (lichat-protocol:from update) user-list) NIL)))

(define-initializer (user-list setup)
  (setf (q+:features user-list) (q+:qdockwidget.dock-widget-movable))
  (setf (q+:window-title user-list) "Users"))

(define-subwidget (user-list list)
    (make-instance 'qui:listing :draggable NIL
                                :sorting (lambda (a b)
                                           (or (and (eql (standing a) :friend)
                                                    (not (eql (standing a) :friend)))
                                               (string< (name a) (name b))))))

(define-subwidget (user-list scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:widget scroller) list))

(define-subwidget (user-list filter)
    (q+:make-qlineedit)
  (setf (q+:placeholder-text filter) "Filter users..."))

(define-subwidget (user-list clear)
    (q+:make-qpushbutton "x")
  (setf (q+:fixed-width clear) 20))

(define-subwidget (user-list center)
    (q+:make-qwidget)
  (setf (q+:widget user-list) center)
  (setf (q+:size-policy center) (values (q+:qsizepolicy.maximum)
                                        (q+:qsizepolicy.minimum))))

(define-subwidget (user-list layout)
    (q+:make-qgridlayout center)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 5)
  (q+:add-widget layout scroller 0 0 1 2)
  (q+:add-widget layout filter 1 0 1 1)
  (q+:add-widget layout clear 1 1 1 1))

(define-slot (user-list filter) ((string string))
  (declare (connected filter (text-changed string)))
  (qui:do-widgets (widget list)
    (let ((name (name (qui:widget-item widget))))
      (setf (q+:visible widget) (search string name :test #'char-equal)))))

(define-slot (user-list clear-filter) ()
  (declare (connected clear (clicked)))
  (setf (q+:text filter) ""))

(defmethod update-listing ((user-list user-list))
  (let ((list (slot-value user-list 'list)))
    (qui:clear-layout list T)
    (when (channel user-list)
      (dolist (user (users (channel user-list)))
        (qui:add-item user list)))))
