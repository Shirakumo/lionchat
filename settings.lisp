#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defun layout-data ()
  (let ((file (ubiquitous:designator-pathname 'layout "dat")))
    (when (probe-file file)
      (let ((file (q+:make-qfile (uiop:native-namestring file))))
        (when (q+:open file (q+:qiodevice.read-only))
          (unwind-protect (q+:read-all file)
            (q+:close file)))))))

(defun (setf layout-data) (bytes)
  (let* ((file (ubiquitous:designator-pathname 'layout "dat"))
         (file (q+:make-qfile (uiop:native-namestring file))))
    (when (q+:open file (q+:qiodevice.write-only))
      (unwind-protect (q+:write file bytes)
        (q+:close file)))))

;; Handle colours
(ubiquitous:define-ubiquitous-writer qobject (object)
  (list (qt::qclass-name (qt::qobject-class object))
        (qtypecase object
          ("QColor" (q+:name object))
          (T (error "Don't know how to serialise qobject ~a." object)))))

(ubiquitous:define-ubiquitous-reader qobject (form)
  (cond ((string= (first form) "QColor")
         (second form))
        (T (error "Don't know how to restore qobject ~a." form))))

(defun default-configuration ()
  (ubiquitous:restore 'main)
  (ubiquitous:defaulted-value (make-hash-table :test 'equal) :connections)
  (ubiquitous:defaulted-value "localhost" :connections :default :hostname)
  (ubiquitous:defaulted-value lichat-tcp-client:*default-port* :connections :default :port)
  (ubiquitous:defaulted-value (machine-user) :connections :default :username)
  (ubiquitous:defaulted-value "" :connections :default :password)
  (ubiquitous:defaulted-value NIL :connections :default :auto)
  (ubiquitous:defaulted-value T :behavior :tray)
  (ubiquitous:defaulted-value T :behavior :notify)
  (ubiquitous:defaulted-value T :behavior :sound)
  (ubiquitous:defaulted-value "" :behaviour :mark)
  (ubiquitous:defaulted-value (q+:make-qcolor "#EEEEEE") :style :text)
  (ubiquitous:defaulted-value (q+:make-qcolor "#CCCCCC") :style :time)
  (ubiquitous:defaulted-value (q+:make-qcolor "#2222EE") :style :link)
  (ubiquitous:defaulted-value (q+:make-qcolor "#FF0000") :style :error)
  (ubiquitous:defaulted-value (q+:make-qcolor "#00FF00") :style :update)
  (ubiquitous:defaulted-value (q+:make-qcolor "#FFFF00") :style :mark)
  (ubiquitous:defaulted-value "h:m:s" :style :format))

(define-widget settings (QDialog)
  ((behavior :initform (make-instance 'behavior-settings))
   (style :initform (make-instance 'style-settings))))

(defmethod initialize-instance :after ((settings settings) &key tab)
  (let ((tabs (slot-value settings 'tabs)))
    (dotimes (i (q+:count tabs))
      (when (string= tab (q+:tab-text tabs i))
        (setf (q+:current-index tabs) i)
        (return)))))

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
  (setf (q+:window-title settings) "Lionchat Settings")
  (q+:resize settings 600 400)
  (q+:add-widget layout tabs 0 0 1 2)
  (q+:add-widget layout ok 1 0 1 1)
  (q+:add-widget layout cancel 1 1 1 1))

(defmethod save ((settings settings))
  (with-slots-bound (settings settings)
    (save behavior)
    (save style)
    (save connections-settings))
  settings)

(defmacro define-settings (name &body options)
  `(progn
     (qui:define-configurable ,name ()
       ,(loop for option in options
              collect (destructuring-bind (path option &rest option-args) option
                        (let ((name (intern (string (car (last path))))))
                          `(,name
                            :initarg ,(car (last path))
                            :initform (ubiquitous:value ,@path)
                            :option (,option ,@option-args))))))
     (defmethod save ((settings ,name))
       (with-slots-bound (settings ,name)
         ,@(loop for option in options
                 for path = (first option)
                 collect `(setf (ubiquitous:value ,@path)
                                ,(intern (string (car (last path)))))))
       settings)))

(define-settings behavior-settings
  ((:behavior :tray) qui:boolean :title "Minimize to Tray")
  ((:behavior :notify) qui:boolean :title "Desktop Notifications")
  ((:behavior :sound) qui:boolean :title "Sound Notifications")
  ((:behavior :mark) qui:string :title "Regex to Highlight"))

(define-settings style-settings
  ((:style :text) qui:color :title "Text Colour")
  ((:style :time) qui:color :title "Timestamp Colour")
  ((:style :link) qui:color :title "Link Colour")
  ((:style :error) qui:color :title "Error Colour")
  ((:style :update) qui:color :title "Update Colour")
  ((:style :mark) qui:color :title "Highlight Colour")
  ((:style :format) qui:string :title "Timestamp Format"))

(define-settings connection-settings
  ((:connections :default :name) qui:string :title "Name")
  ((:connections :default :hostname) qui:string :title "Hostname")
  ((:connections :default :port) qui:integer :title "Port" :min 1 :max 65535)
  ((:connections :default :username) qui:string :title "Username")
  ((:connections :default :password) qui:password :title "Password")
  ((:connections :default :auto) qui:boolean :title "Autoconnect"))

;; Special treatment due to multiplexing by name.
(defmethod save ((settings connection-settings))
  (with-slots-bound (settings connection-settings)
    (setf (ubiquitous:value :connections name :name) name)
    (setf (ubiquitous:value :connections name :hostname) hostname)
    (setf (ubiquitous:value :connections name :port) port)
    (setf (ubiquitous:value :connections name :username) username)
    (setf (ubiquitous:value :connections name :password) password)
    (setf (ubiquitous:value :connections name :auto) auto)))

(define-widget connections-settings (QWidget)
  ((connections :initform () :accessor connections)))

(define-initializer (connections-settings setup)
  (setf (connections connections-settings)
        (loop for connection being the hash-values of (ubiquitous:value :connections)
              when (stringp (gethash :name connection))
              collect (apply #'make-instance 'connection-settings
                             (alexandria:hash-table-plist connection)))))

(define-subwidget (connections-settings list) (q+:make-qlistwidget))

(define-subwidget (connections-settings add) (q+:make-qpushbutton "+")
  (setf (q+:default add) T)
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

(defun sname (object)
  (slot-value object 'name))

(defmethod (setf connections) :around (connections (connections-settings connections-settings))
  (call-next-method (sort connections #'string< :key #'sname) connections-settings))

(defmethod (setf connections) :after (connections (connections-settings connections-settings))
  (with-slots-bound (connections-settings connections-settings)
    (let ((row (q+:current-row list)))
      (q+:clear list)
      (dolist (connection connections)
        (q+:add-item list (slot-value connection 'name)))
      (setf (q+:current-row list) (max 0 (min row (1- (length connections))))))))

(define-slot (connections-settings current) ((name string))
  (declare (connected list (current-text-changed string)))
  (let ((prev main)
        (new (find name connections :key #'sname :test #'string=)))
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
                    :key #'sname :test #'string=)))))

(defmethod save ((settings connections-settings))
  (with-slots-bound (settings connections-settings)
    (mapc #'save connections)
    (loop for connection being the hash-values of (ubiquitous:value :connections)
          for name = (gethash :name connection)
          when (and (stringp name) (not (find name connections
                                              :key (lambda (A) (slot-value a 'name)))))
          do (ubiquitous:remvalue :connections name)))
  settings)
