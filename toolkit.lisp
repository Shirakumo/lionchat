#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defmacro setf-named (list name new)
  (let ((newg (gensym "NEW")) (listg (gensym "LIST"))
        (nameg (gensym "NAME")) (prevg (gensym "PREV")))
    `(let* ((,newg ,new)
            (,listg ,list)
            (,nameg ,name)
            (,prevg (find ,nameg ,listg :key #'name :test #'string=)))
       (unless (eql ,newg ,prevg)
         (setf ,listg (remove ,prevg ,listg))
         (when ,newg
           (setf ,listg (sort (list* ,newg ,listg)
                              #'string< :key #'name)))
         (setf ,list ,listg))
       ,newg)))

(defun machine-user ()
  (let ((path (user-homedir-pathname)))
    (if (cdr (pathname-directory path))
        (car (last (pathname-directory path)))
        (machine-instance))))

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

(defun escape-html (html)
  (with-output-to-string (stream)
    (loop for c across html
          do (case c
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\" (write-string "&quot;" stream))
               (#\& (write-string "&amp;" stream))
               (#\Newline (write-string "<br>" stream))
               (T (write-char c stream))))))

(defun object-color (object)
  (let* ((hash (sxhash object))
         (color (logand hash #xFFFFFF))
         (red (ldb (byte 8 16) color))
         (green (ldb (byte 8 8) color))
         (blue (ldb (byte 8 0) color)))
    ;; Make sure to constrain the range of the colours at least a
    ;; little bit with the hope to avoid making colours that are
    ;; completely unreadable for some backgrounds.
    (format NIL "#~2,'0x~2,'0x~2,'0x"
            (min 200 (max 50 red))
            (min 200 (max 50 green))
            (min 200 (max 50 blue)))))

(defun permissions-string (perms)
  (with-output-to-string (out)
    (let ((*print-case* :downcase)
          (*package* (find-package :org.shirakumo.lichat.protocol)))
      (format out "(~{~s~^~% ~})" perms))))

(defun read-permissions (string)
  (let ((*package* (find-package :org.shirakumo.lichat.protocol)))
    (read-from-string string)))

(defun show-error (parent text &rest args)
  (q+:qmessagebox-warning
   parent "Lionchat Error"
   (escape-html (apply #'format NIL text args))))
