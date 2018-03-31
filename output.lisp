#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(define-widget output (QScrollArea)
  ((buffer :initarg :buffer :accessor buffer)))

(define-subwidget (output widget) (q+:make-qwidget)
  (setf (q+:widget-resizable output) T)
  (setf (q+:vertical-scroll-bar-policy output) (q+:qt.scroll-bar-always-on))
  (setf (q+:horizontal-scroll-bar-policy output) (q+:qt.scroll-bar-always-off))
  (setf (q+:size-policy output) (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.minimum))))

(define-subwidget (output layout) (q+:make-qvboxlayout widget)
  ;; For whatever reason you must do this after the layout has been added.
  ;; Otherwise, the widget will be invisible forever. Thanks, Qt.
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (setf (q+:widget output) widget))

(defmethod (setf buffer) :after (buffer (output output))
  (refresh output))

(defmethod refresh ((output output) &key (offset 0))
  (let ((layout (slot-value output 'layout))
        (buffer (buffer output)))
    ;; (loop repeat (- (q+:count layout) offset)
    ;;       for item = (q+:take-at layout 0)
    ;;       until (typep item 'null-qobject)
    ;;       do (q+:remove-item layout item)
    ;;          (finalize (q+:widget item)))
    (let ((spacer (q+:take-at layout (1- (q+:count layout)))))
      (q+:remove-item layout spacer)
      (finalize spacer))
    (when buffer
      (loop with items = (items buffer)
            for i from offset below (length items)
            do (q+:add-widget layout (item->widget (aref items i)))))
    (q+:add-stretch layout)))
