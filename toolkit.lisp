#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defun format-time (stamp &optional (format "h:m:s"))
  (let ((stamp (etypecase stamp
                 (local-time:timestamp stamp)
                 (integer (local-time:universal-to-timestamp stamp)))))
    (local-time:format-timestring NIL stamp :format (parse-time-format format))))

(defun parse-time-format (format)
  (loop for char across format
        collect (case char
                  (#\Y '(:year 4))
                  (#\M '(:month 2))
                  (#\D '(:day 2))
                  (#\h '(:hour 2))
                  (#\m '(:min 2))
                  (#\s '(:sec 2))
                  (#\d :short-weekday)
                  (#\n :short-month)
                  (T char))))
