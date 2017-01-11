#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget tray (QSystemTrayIcon)
  ((main :initarg :main :accessor main)))

(define-subwidget (tray show) (q+:make-qaction "&Show/Hide" tray))

(define-subwidget (tray settings) (q+:make-qaction "&Settings" tray)
  (connect settings "triggered()" (main tray) "m_SettingsActionSlot()"))

(define-subwidget (tray quit) (q+:make-qaction "&Quit" tray)
  (connect quit "triggered()" (main tray) "m_QuitActionSlot()"))

(define-subwidget (tray menu) (q+:make-qmenu)
  (q+:add-action menu show)
  (q+:add-action menu settings)
  (q+:add-separator menu)
  (q+:add-action menu quit))

(define-initializer (tray setup 0)
  (setf (q+:tool-tip tray) "Lionchat")
  ;; (setf (q+:icon tray) (q+:make-qicon (uiop:native-namestring (resource "tray.png"))))
  (setf (q+:context-menu tray) menu))

(define-signal (tray tray-message) (string string))

(define-slot (tray tray-message) ((title string) (message string))
  (declare (connected tray (tray-message string string)))
  (q+:show-message tray title message (q+:qsystemtrayicon.no-icon) 5000))

(defmethod tray-message ((tray tray) title message)
  (signal! tray (tray-message string string) title message))

(define-slot (tray activated) ((reason "QSystemTrayIcon::ActivationReason"))
  (declare (connected tray (activated "QSystemTrayIcon::ActivationReason")))
  ;; (when (= 3 reason)
  ;;   (setf (q+:visible (main tray)) (not (q+:is-visible (main tray)))))
  )

(define-slot (tray show) ()
  (declare (connected show (triggered)))
  (setf (q+:visible (main tray)) (not (q+:is-visible (main tray)))))
