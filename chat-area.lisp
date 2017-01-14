#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget chat-area (QWidget)
  ((main :initarg :main :accessor main)
   (channel :initform NIL :accessor channel)))

(defmethod (setf channel) :after (thing (chat-area chat-area))
  (update (slot-value chat-area 'output) thing)
  (setf (q+:enabled (slot-value chat-area 'send)) thing))

;; Only update if the client (and channel) match the update.
(defmethod update ((chat-area chat-area) (update lichat-protocol:update))
  (when (and (channel chat-area)
             (eql (client (channel chat-area)) (client update)))
    (update (slot-value chat-area 'output) update)))

(defmethod update ((chat-area chat-area) (update lichat-protocol:channel-update))
  (when (and (channel chat-area)
             (eql (client (channel chat-area)) (client update))
             (string= (name (channel chat-area)) (lichat-protocol:channel update)))
    (update (slot-value chat-area 'output) update)))

(define-subwidget (chat-area output)
    (make-instance 'chat-output))

(define-subwidget (chat-area input-area)
    (q+:make-qwidget)
  (setf (q+:fixed-height input-area) 75))

(define-subwidget (chat-area input)
    (make-instance 'chat-input))

(define-subwidget (chat-area send)
    (q+:make-qpushbutton "Send")
  (setf (q+:size-policy send) (values (q+:qsizepolicy.maximum)
                                      (q+:qsizepolicy.expanding)))
  (setf (q+:enabled send) NIL))

(define-subwidget (chat-area layout)
    (q+:make-qvboxlayout chat-area)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 5)
  (q+:add-widget layout output)
  (let ((sub (q+:make-qhboxlayout input-area)))
    (setf (q+:margin sub) 0)
    (setf (q+:spacing sub) 0)
    (q+:add-widget sub input)
    (q+:add-widget sub send))
  (q+:add-widget layout input-area))

(define-slot (chat-area send) ()
  (declare (connected send (clicked)))
  (declare (connected input (confirmed)))
  (when (channel chat-area)
    (let ((text (q+:to-plain-text input))
          (channel (channel chat-area)))
      (cond ((string= "" text))
            ((starts-with "/" text)
             (run-command channel (subseq text 1)))
            (T
             (qsend channel
                    'lichat-protocol:message
                    :channel (name channel)
                    :text text))))
    (setf (q+:plain-text input) "")))

(define-widget chat-output (QTextBrowser)
  ())

(define-initializer (chat-output setup)
  (setf (q+:text-interaction-flags chat-output) (q+:qt.links-accessible-by-mouse))
  (setf (q+:open-external-links chat-output) T)
  (setf (q+:read-only chat-output) T))

(define-subwidget (chat-output font) (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font chat-output) font))

(defmethod update ((chat-output chat-output) (null null))
  (q+:clear chat-output)
  (setf (q+:default-style-sheet (q+:document chat-output))
        (format NIL "a{color:~a} *{white-space:pre-wrap;} mark{background:~a;color:~a}"
                (ubiquitous:value :style :link)
                (ubiquitous:value :style :mark)
                (invert-color (ubiquitous:value :style :mark)))))

(defmethod update ((chat-output chat-output) (channel channel))
  (update chat-output NIL)
  (update chat-output (updates channel)))

(defmethod show-update ((update lichat-protocol:update) (stream stream))
  (format stream "<span style=\"color:~a\">~a</span> ~
                  <span style=\"color:~a\">* UPDATE</span> ~a<br>"
          (ubiquitous:value :style :time) (format-time (lichat-protocol:clock update))
          (ubiquitous:value :style :update) (type-of update)))

(defmethod show-update ((update lichat-protocol:ping) (stream stream)))
(defmethod show-update ((update lichat-protocol:permissions) (stream stream)))

(defmethod show-update ((update lichat-protocol:failure) (stream stream))
  (format stream "<span style=\"color:~a\">~a</span> ~
                  <span style=\"color:~a\">* ~a</span> ~a<br>"
          (ubiquitous:value :style :time) (format-time (lichat-protocol:clock update))
          (ubiquitous:value :style :error) (type-of update) (escape-html (lichat-protocol:text update))))

(defmethod show-update ((update lichat-protocol:message) (stream stream))
  (format stream "<span style=\"color:~a\">~a</span> ~
                  <span style=\"color:~a\" title=\"~a\">~a</span>: ~
                  <span style=\"color:~a\">~a</span><br>"
          (ubiquitous:value :style :time) (format-time (lichat-protocol:clock update))
          (object-color (lichat-protocol:from update))
          (lichat-protocol:from update) (format-name (lichat-protocol:from update))
          (ubiquitous:value :style :text)
          (format-update-text update)))

(defun show-update-info (update stream src msg &rest args)
  (format stream "<span style=\"color:~a\">~a</span> ~
                  * <span style=\"color:~a\">~a</span> ~?<br>"
          (ubiquitous:value :style :time) (format-time (lichat-protocol:clock update))
          (object-color src) src msg args))

(defun show-update-action (update stream msg &rest args)
  (apply #'show-update-info update stream (lichat-protocol:from update) msg args))

(defmethod show-update ((update lichat-protocol:users) (stream stream))
  (show-update-info update stream (lichat-protocol:channel update) "users: ~{~a~^, ~}"
                    (lichat-protocol:users update)))

(defmethod show-update ((update lichat-protocol:channels) (stream stream))
  (show-update-info update stream (lichat-protocol:from update) "channels: ~{~a~^, ~}"
                    (lichat-protocol:channels update)))

(defmethod show-update ((update lichat-protocol:user-info) (stream stream))
  (show-update-action update stream "is ~:[not registered~;registered~] and has ~d connection~:p"
                      (lichat-protocol:registered update) (lichat-protocol:connections update)))

(defmethod show-update ((update lichat-protocol:join) (stream stream))
  (show-update-action update stream "joined"))

(defmethod show-update ((update lichat-protocol:leave) (stream stream))
  (show-update-action update stream "left"))

(defmethod show-update ((update lichat-protocol:kick) (stream stream))
  (show-update-action update stream "kicked <span style=\"color:~a\">~a</a>"
                      (object-color (lichat-protocol:target update)) (lichat-protocol:target update)))

(defmethod show-update ((update lichat-protocol:pull) (stream stream))
  (show-update-action update stream "pulled <span style=\"color:~a\">~a</a>"
                      (object-color (lichat-protocol:target update)) (lichat-protocol:target update)))

(defmethod show-update ((update lichat-protocol:text-update) (stream stream))
  (show-update-info update stream (lichat-protocol:from update) "~a"
                    (lichat-protocol:text update)))

(defmethod show-update :around ((update lichat-protocol:update) (stream stream))
  (let ((user (find-user (lichat-protocol:from update) (client update))))
    (cond (user
           (unless (muted-p user) (call-next-method)))
          ((not (ubiquitous:value :users (name (client update)) (lichat-protocol:from update) :muted))
           (call-next-method)))))

(defmethod update ((chat-output chat-output) (update lichat-protocol:update))
  (q+:insert-html chat-output
                  (with-output-to-string (out)
                    (show-update update out)))
  (q+:move-cursor chat-output (q+:qtextcursor.end))
  (q+:ensure-cursor-visible chat-output))

(defmethod update ((chat-output chat-output) (updates vector))
  (q+:insert-html chat-output
                  (with-output-to-string (out)
                    (loop for update across updates
                          do (show-update update out))))
  (q+:move-cursor chat-output (q+:qtextcursor.end))
  (q+:ensure-cursor-visible chat-output))

(define-widget chat-input (QPlainTextEdit)
  ())

(define-signal (chat-input confirmed) ())

(define-override (chat-input key-press-event) (ev)
  (when (and (enum-equal (q+:key ev) (q+:qt.key_return))
             (enum-equal (q+:modifiers ev) (q+:qt.control-modifier)))
    (signal! chat-input (confirmed)))
  (stop-overriding))
