#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget connect (QDialog)
  ())

(defmethod initialize-instance :after ((connect connect) &key hostname port username password)
  (setf (q+:text (slot-value connect 'hostname)) hostname)
  (setf (q+:value (slot-value connect 'port)) port)
  (setf (q+:text (slot-value connect 'username)) username)
  (setf (q+:text (slot-value connect 'password)) password))

(define-subwidget (connect hostname)
    (q+:make-qlineedit connect))

(define-subwidget (connect port)
    (q+:make-qspinbox lichat:*default-port*)
  (setf (q+:minimum port) 1)
  (setf (q+:maximum port) 65535))

(define-subwidget (connect username)
    (q+:make-qlineedit))

(define-subwidget (connect password)
    (q+:make-qlineedit)
  (setf (q+:echo-mode password) (q+:qlineedit.password)))

(define-subwidget (connect ok)
    (q+:make-qpushbutton "Connect")
  (connect! ok (clicked) connect (accept)))

(define-subwidget (connect cancel)
    (q+:make-qpushbutton "Cancel")
  (connect! cancel (clicked) connect (reject)))

(define-subwidget (connect layout)
    (q+:make-qgridlayout)
  (let ((form (q+:make-qformlayout)))
    (q+:add-row form "Hostname" hostname)
    (q+:add-row form "Port" port)
    (q+:add-row form "Username" username)
    (q+:add-row form "Password" password)
    (q+:add-layout layout form 0 0 1 2))
  (q+:add-widget layout ok 1 0 1 1)
  (q+:add-widget layout cancel 1 1 1 1))

(defmethod settings ((connect connect))
  (with-slots-bound (connect connect)
    (list :hostname (q+:text hostname)
          :port (q+:value port)
          :username (q+:text username)
          :password (q+:text password))))
