#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defvar *deployed* NIL)

(pushnew 'deploy-resources qtools:*build-hooks*)

(defun deploy-resources ()
  (let ((target (merge-pathnames "data/" qtools:*deployment-location*)))
    (ensure-directories-exist target)
    (dolist (file (directory (asdf:system-relative-pathname :lionchat "data/*.*")))
      (uiop:copy-file file (make-pathnames :name (pathname-name file)
                                           :type (pathname-type file)
                                           :defaults target))))
  (setf *deployed* T))

(defun data (file)
  (merge-pathnames file (if *deployed*
                            (merge-pathnames "data/" (uiop:argv0))
                            (asdf:system-relative-pathname :lionchat "data/"))))

(defun starts-with (prefix string &key (start 0))
  (and (<= (length prefix) (+ start (length string)))
       (string= prefix string :start2 start :end2 (+ start (length prefix)))))

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

(defun regex-escape (text)
  (cl-ppcre:regex-replace-all
   "([\\.\\*\\?\\|\\[\\]\\(\\)\\{\\}\\+\\-\\\\])" text "\\\\\\1"))

(defun mark (text regex)
  (cl-ppcre:regex-replace-all
   (format NIL "(~a)" regex)
   text
   "<mark>\\1</mark>"))

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

(defun format-update-text (update)
  (linkify-urls (mark (escape-html (lichat-protocol:text update))
                      (format NIL "~a|~a"
                              (regex-escape (username (client update)))
                              (ubiquitous:value :behavior :mark)))))

(defun object-color (object)
  (let* ((hash (sxhash object))
         (color (logand hash #xFFFFFF))
         (r (ldb (byte 8 16) color))
         (g (ldb (byte 8 8) color))
         (b (ldb (byte 8 0) color)))
    ;; Make sure to constrain the range of the colours at least a
    ;; little bit with the hope to avoid making colours that are
    ;; completely unreadable for some backgrounds.
    (format NIL "#~2,'0x~2,'0x~2,'0x"
            (min 200 (max 50 r))
            (min 200 (max 50 g))
            (min 200 (max 50 b)))))

(defun invert-color (name)
  (let ((r (parse-integer name :start 1 :end 3 :radix 16))
        (g (parse-integer name :start 3 :end 5 :radix 16))
        (b (parse-integer name :start 5 :end 7 :radix 16)))
    (format NIL "#~2,'0x~2,'0x~2,'0x"
            (- 255 r) (- 255 g) (- 255 b))))

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

;; Define early.
(defmacro with-awaiting ((update id main) &body body)
  (let ((idg (gensym "ID")))
    `(let ((,idg ,id))
       (push (lambda (,update)
               (when (or (equal ,idg (lichat-protocol:id ,update))
                         (equal ,idg (lichat-protocol:update-id ,update)))
                 ,@body
                 T))
             (awaiting ,main)))))
