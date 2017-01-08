#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(define-widget chat-area (QWidget)
  ((main :initarg :main :accessor main)
   (channel :initarg :channel :accessor channel)))

(defmethod (setf channel) :after ((channel channel) (chat-area chat-area))
  (update (slot-value chat-area 'output) channel))

(defmethod update ((chat-area chat-area) thing)
  (update (slot-value chat-area 'output) thing))

(define-subwidget (chat-area output)
    (make-instance 'chat-output))

(define-subwidget (chat-area input-area)
    (q+:make-qwidget)
  (setf (q+:fixed-height input-area) 100))

(define-subwidget (chat-area input)
    (make-instance 'chat-input))

(define-subwidget (chat-area send)
    (q+:make-qpushbutton "Send")
  (setf (q+:size-policy send) (values (q+:qsizepolicy.maximum)
                                      (q+:qsizepolicy.expanding))))

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
  (qsend (client (main chat-area))
         'lichat-protocol:message
         :channel (name (channel chat-area))
         :text (q+:to-plain-text input))
  (setf (q+:plain-text input) ""))

(define-widget chat-output (QTextBrowser)
  ())

(define-initializer (chat-output setup)
  (setf (q+:text-interaction-flags chat-output) (q+:qt.links-accessible-by-mouse))
  (setf (q+:open-external-links chat-output) T)
  (setf (q+:read-only chat-output) T))

(define-subwidget (chat-output font) (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font chat-output) font))

(defmethod update ((chat-output chat-output) (channel channel))
  (q+:clear chat-output)
  (update chat-output (updates channel)))

(defmethod show-update ((update lichat-protocol:update) (stream stream))
  (format stream "[UPDATE: ~a]<br>" (type-of update)))

(defmethod show-update ((update lichat-protocol:message) (stream stream))
  (format stream "<span style=\"color:~a\">~a</span> ~
                  <span style=\"color:~a\">~a:</span> ~
                  <span style=\"color:~a;display: inline-block\">~a</span><br>"
          "#CCC" (format-time (lichat-protocol:clock update))
          "#0088EE"
          (lichat-protocol:from update)
          "#EEE" (cl-ppcre:regex-replace-all "\\n" (lichat-protocol:text update) "<br>")))

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
