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

(define-subwidget (settings connections-settings)
    (make-instance 'connections-settings))

(define-subwidget (settings tabs)
    (q+:make-qtabwidget)
  (setf (q+:tabs-closable tabs) NIL)
  (q+:add-tab tabs behavior-widget "Behaviour")
  (q+:add-tab tabs style-widget "Style")
  (q+:add-tab tabs connections-settings "Connections"))

(define-subwidget (settings ok)
    (q+:make-qpushbutton "Save")
  (setf (q+:default ok) T)
  (connect! ok (clicked) settings (accept)))

(define-subwidget (settings cancel)
    (q+:make-qpushbutton "Cancel")
  (connect! cancel (clicked) settings (reject)))

(define-subwidget (settings layout)
    (q+:make-qgridlayout settings)
  (q+:resize settings 600 400)
  (q+:add-widget layout tabs 0 0 1 2)
  (q+:add-widget layout ok 1 0 1 1)
  (q+:add-widget layout cancel 1 1 1 1))

(defmethod settings ((settings settings))
  (with-slots-bound (settings settings)
    `((:behavior .,(settings behavior))
      (:style .,(settings style))
      (:connection .,(settings connections-settings)))))

(qui:define-configurable behavior-settings ()
  ((tray :initarg :tray :option (boolean :title "Minimize to Tray"))
   (sound :initarg :sound :option (boolean :title "Play Sounds")))
  (:default-initargs
   :tray (ubiquitous:value :behavior :tray)
   :sound (ubiquitous:value :behavior :sound)))

(defmethod settings ((settings behavior-settings))
  (with-slots-bound (settings behavior-settings)
    `((:tray .,tray)
      (:sound .,sound))))

(qui:define-configurable style-settings ()
  ((text :initarg :text :option (qui:color :title "Text Colour"))
   (time :initarg :time :option (qui:color :title "Timestamp Colour"))
   (link :initarg :link :option (qui:color :title "Link Colour"))
   (error :initarg :error :option (qui:color :title "Error Colour"))
   (update :initarg :update :option (qui:color :title "Update Colour"))
   (format :initarg :format :option (string :title "Timestamp Format")))
  (:default-initargs
   :text (ubiquitous:value :style :text)
   :time (ubiquitous:value :style :time)
   :link (ubiquitous:value :style :link)
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
        (:link .,(convert-color link))
        (:error .,(convert-color error))
        (:update .,(convert-color update))
        (:format .,format)))))

(define-widget connections-settings (QWidget)
  ((connections :initform () :accessor connections)))

(define-initializer (connections-settings setup)
  (setf (connections connections-settings)
        (loop for connection being the hash-values of (ubiquitous:value :connection)
              when (listp connection)
              collect (apply #'make-instance 'connection-settings
                             (alexandria:alist-plist connection)))))

(define-subwidget (connections-settings list) (q+:make-qlistwidget))

(define-subwidget (connections-settings add) (q+:make-qpushbutton "+")
  (setf (q+:minimum-width add) 20)
  (setf (q+:size-policy add) (values (q+:qsizepolicy.maximum)
                                     (q+:qsizepolicy.maximum))))

(define-subwidget (connections-settings remove) (q+:make-qpushbutton "-")
  (setf (q+:minimum-width remove) 20)
  (setf (q+:size-policy remove) (values (q+:qsizepolicy.maximum)
                                      (q+:qsizepolicy.maximum))))

(define-subwidget (connections-settings main) (q+:make-qwidget)
  (setf (q+:size-policy main) (values (q+:qsizepolicy.minimum)
                                      (q+:qsizepolicy.minimum))))

(define-subwidget (connections-settings layout) (q+:make-qgridlayout connections-settings)
  (q+:add-widget layout list   0 0 1 2)
  (q+:add-widget layout add    1 0 1 1)
  (q+:add-widget layout remove 1 1 1 1)
  (q+:add-widget layout main   0 2 2 1)
  (setf (q+:column-stretch layout) (values 2 1)))

(defmethod (setf connections) :around (connections (connections-settings connections-settings))
  (call-next-method (sort connections #'string< :key #'name) connections-settings))

(defmethod (setf connections) :after (connections (connections-settings connections-settings))
  (with-slots-bound (connections-settings connections-settings)
    (let ((row (q+:current-row list)))
      (q+:clear list)
      (dolist (connection connections)
        (q+:add-item list (name connection)))
      (setf (q+:current-row list) (min row (1- (length connections)))))))

(define-slot (connections-settings current) ((name string))
  (declare (connected list (current-text-changed string)))
  (let ((prev main)
        (new (find name connections :key #'name :test #'string=)))
    (when new
      (setf main (qui:configuration-container new))
      (q+:remove-widget layout prev)
      (finalize prev)
      (q+:add-widget layout main 0 2 2 1))))

(define-slot (connections-settings add) ()
  (declare (connected add (clicked)))
  (push (make-instance 'connection-settings :name "")
        (connections connections-settings)))

(define-slot (connections-settings remove) ()
  (declare (connected remove (clicked)))
  (let ((current (q+:current-item list)))
    (unless (null-qobject-p current)
      (setf (connections connections-settings)
            (remove (q+:text current) (connections connections-settings)
                    :key #'name :test #'string=)))))

(defmethod settings ((settings connections-settings))
  (with-slots-bound (settings connections-settings)
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash :default table) (ubiquitous:value :connection :default))
      (dolist (connection connections table)
        (setf (gethash (name connection) table)
              (settings connection))))))

(qui:define-configurable connection-settings ()
  ((name :initarg :name :accessor name :option (qui:string :title "Name"))
   (hostname :initarg :hostname :option (qui:string :title "Hostname"))
   (port :initarg :port :option (qui:integer :title "Port" :min 1 :max 65535))
   (username :initarg :username :option (qui:string :title "Username"))
   (password :initarg :password :option (qui:password :title "Password"))
   (auto :initarg :auto :option (qui:boolean :title "Autoconnect")))
  (:default-initargs
   :name ""
   :hostname (ubiquitous:value :connection :default :hostname)
   :port (ubiquitous:value :connection :default :port)
   :username (ubiquitous:value :connection :default :username)
   :password (ubiquitous:value :connection :default :password)
   :auto (ubiquitous:value :connection :default :auto)))

(defmethod settings ((settings connection-settings))
  (with-slots-bound (settings connection-settings)
    `((:name .,name)
      (:hostname .,hostname)
      (:port .,port)
      (:username .,username)
      (:password .,password)
      (:auto .,auto))))

(defun default-configuration ()
  (ubiquitous:restore :lionchat)
  (ubiquitous:defaulted-value "localhost" :connection :default :hostname)
  (ubiquitous:defaulted-value lichat-tcp-client:*default-port* :connection :default :port)
  (ubiquitous:defaulted-value (machine-user) :connection :default :username)
  (ubiquitous:defaulted-value "" :connection :default :password)
  (ubiquitous:defaulted-value NIL :connection :default :auto)
  (ubiquitous:defaulted-value T :behavior :tray)
  (ubiquitous:defaulted-value T :behavior :sound)
  (ubiquitous:defaulted-value "#EEE" :style :text)
  (ubiquitous:defaulted-value "#CCC" :style :time)
  (ubiquitous:defaulted-value "#22E" :style :link)
  (ubiquitous:defaulted-value "#F00" :style :error)
  (ubiquitous:defaulted-value "#0F0" :style :update)
  (ubiquitous:defaulted-value "h:m:s" :style :format))
