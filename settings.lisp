#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget settings (QDialog)
  ((behavior :initform (make-instance 'behavior-settings))
   (style :initform (make-instance 'style-settings))))

(define-subwidget (settings behavior-widget)
    (qui:configuration-container behavior))

(define-subwidget (settings style-widget)
    (qui:configuration-container style))

(define-subwidget (settings tabs)
    (q+:make-qtabwidget)
  (setf (q+:tabs-closable tabs) NIL)
  (q+:add-tab tabs behavior-widget "Behaviour")
  (q+:add-tab tabs style-widget "Style"))

(define-subwidget (settings ok)
    (q+:make-qpushbutton "Save")
  (setf (q+:default ok) T)
  (connect! ok (clicked) settings (accept)))

(define-subwidget (settings cancel)
    (q+:make-qpushbutton "Cancel")
  (connect! cancel (clicked) settings (reject)))

(define-subwidget (settings layout)
    (q+:make-qgridlayout settings)
  (q+:add-widget layout tabs 0 0 1 2)
  (q+:add-widget layout ok 1 0 1 1)
  (q+:add-widget layout cancel 1 1 1 1))

(defmethod settings ((settings settings))
  (with-slots-bound (settings settings)
    `((:behavior .,(settings behavior))
      (:style .,(settings style)))))

(qui:define-configurable behavior-settings ()
  ((tray :initarg :tray :option (boolean :title "Minimize to Tray"))
   (connect :initarg :connect :option (boolean :title "Connect on Startup"))
   (sound :initarg :sound :option (boolean :title "Play Sounds")))
  (:default-initargs
   :tray (ubiquitous:value :behavior :tray)
   :connect (ubiquitous:value :behavior :connect)
   :sound (ubiquitous:value :behavior :sound)))

(defmethod settings ((settings behavior-settings))
  (with-slots-bound (settings behavior-settings)
    `((:tray .,tray)
      (:connect .,connect)
      (:sound .,sound))))

(qui:define-configurable style-settings ()
  ((text :initarg :text :option (qui:color :title "Text Colour"))
   (time :initarg :time :option (qui:color :title "Timestamp Colour"))
   (error :initarg :error :option (qui:color :title "Error Colour"))
   (update :initarg :update :option (qui:color :title "Update Colour"))
   (format :initarg :format :option (string :title "Timestamp Format")))
  (:default-initargs
   :text (ubiquitous:value :style :text)
   :time (ubiquitous:value :style :time)
   :error (ubiquitous:value :style :error)
   :update (ubiquitous:value :style :update)
   :format (ubiquitous:value :style :format)))

(defmethod settings ((settings style-settings))
  (with-slots-bound (settings style-settings)
    (flet ((convert-color (color)
             (etypecase color
               (string color)
               (qobject
                (format NIL "#~2,'0x~2,'0x~2,'0x"
                        (q+:red color) (q+:green color) (q+:blue color))))))
      `((:text .,(convert-color text))
        (:time .,(convert-color time))
        (:error .,(convert-color error))
        (:update .,(convert-color update))
        (:format .,format)))))

(defun default-configuration ()
  (ubiquitous:restore :lionchat)
  (ubiquitous:defaulted-value T :behavior :tray)
  (ubiquitous:defaulted-value T :behavior :connect)
  (ubiquitous:defaulted-value T :behavior :sound)
  (ubiquitous:defaulted-value "#EEE" :style :text)
  (ubiquitous:defaulted-value "#CCC" :style :time)
  (ubiquitous:defaulted-value "#F00" :style :error)
  (ubiquitous:defaulted-value "#0F0" :style :update)
  (ubiquitous:defaulted-value "h:m:s" :style :format))
