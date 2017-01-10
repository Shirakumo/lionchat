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

(defun format-name (name &optional (length 10))
  (if (<= (length name) length)
      (format NIL "~a~v@{ ~}" name (- length (length name)) T)
      (format NIL "~a~~" (subseq name 0 (1- length)))))

(defparameter *url-regex*
  (cl-ppcre:create-scanner
   "((?:[\\w-_]+://)([\\w_-]+(?:(?:\\.[\\w_-]+)+))(?:[\\w.,@?^=%&:/~+#-()]*[\\w@?^=%&/~+#-])?)"))

(defun linkify-urls (text)
  (cl-ppcre:regex-replace-all
   *url-regex*
   text
   "<a href=\"\\1\">\\1</a>"))
