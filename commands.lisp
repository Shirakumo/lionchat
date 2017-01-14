#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.lionchat)
(in-readtable :qtools)

(defvar *commands* (make-hash-table :test 'equal))

(defclass internal-update (lichat-protocol:text-update)
  ())

(defun command (name)
  (gethash (string-downcase name) *commands*))

(defun (setf command) (function name)
  (setf (gethash (string-downcase name) *commands*) function))

(defun remove-command (name)
  (remhash (string-downcase name) *commands*))

(defun list-commands ()
  (loop for k being the hash-keys of *commands*
        collect k))

(defun command-arglist (name)
  (loop with state = '&required
        for arg in (rest
                    (trivial-arguments:arglist
                     (etypecase name
                       (function name)
                       (string (command name)))))
        if (find arg '(&optional &rest))
        do (setf state arg)
        else
        collect (case state
                  (&required
                   (format NIL "~(~a~)" arg))
                  (&optional
                   (format NIL "[~(~a~)]" (if (listp arg) (first arg) arg)))
                  (&rest
                   (format NIL "...")))))

(defun split-command-string (string)
  (let ((args))
    (with-input-from-string (input string)
      (loop with buffer = (make-string-output-stream)
            for char = (read-char input NIL)
            while char
            do (case char
                 (#\\ (write-char (read-char input) buffer))
                 (#\  (push (get-output-stream-string buffer) args))
                 (T (write-char char buffer)))
            finally (push (get-output-stream-string buffer) args))
      (nreverse args))))

(defun show-update-text (channel text)
  (let ((update (make-instance 'internal-update
                               :from "Lionchat"
                               :text text)))
    (setf (client update) (client channel))
    (update (main (client channel)) update)))

(defun run-command (channel string)
  (let* ((args (split-command-string string))
         (command (command (first args))))
    (handler-case
        (if command
            (handler-case (apply command channel (rest args))
              (error (err)
                (declare (ignore err))
                (error "Invalid syntax. Expected: /~a~{ ~a~}"
                       (first args)
                       (command-arglist command))))
            (error "Unknown command ~a" (first args)))
      (error (err)
        (show-update-text channel (format NIL "<span style=\"color:red\">Error</span>: <span>~a</span>"
                                          (escape-html (princ-to-string err))))))))

(defmacro define-command (prefix (channel &rest args) &body body)
  `(setf (command ,(string prefix))
         (lambda (,channel ,@args)
           ,@body)))

(define-command help (channel)
  "Show all available commands"
  (show-update-text channel
                    (with-output-to-string (o)
                      (format o "The following commands are available:<table>")
                      (loop for name in (list-commands)
                            for command = (command name)
                            for prefix = (format NIL "/~a~{ ~a~}" name (command-arglist command))
                            do (format o "<tr><td style=\"color:orange;padding-right:10px\">~a</td><td>~@[~a~]</td></tr>"
                                       prefix (documentation command T))))))

(define-command create (channel &optional name)
  "Create a new channel. Not specifying a name will create an anonymous channel."
  (qsend channel 'lichat-protocol:create :channel name))

(define-command join (channel name)
  "Join an existing channel."
  (qsend channel 'lichat-protocol:join :channel name))

(define-command leave (channel &optional (name (name channel)))
  "Leave a channel. Not specifying a name will leave the current channel."
  (qsend channel 'lichat-protocol:leave :channel name))

(define-command pull (channel user &optional (name (name channel)))
  "Pull a user into a channel. Not specifying a name will leave the current channel."
  (qsend channel 'lichat-protocol:pull :channel name :target user))

(define-command kick (channel user &optional (name (name channel)))
  "Kick a user from a channel. Not specifying a name will leave the current channel."
  (qsend channel 'lichat-protocol:kick :channel name :target user))

(define-command users (channel &optional (name (name channel)))
  "Fetch a list of users from a channel. Not specifying a name will leave the current channel."
  (qsend channel 'lichat-protocol:users :channel name))

(define-command channels (channel)
  "Fetch a list of public channels."
  (qsend channel 'lichat-protocol:channels))

(define-command info (channel user)
  "Fetch information about a user."
  (qsend channel 'lichat-protocol:user-info :target user))

(define-command message (channel name &rest text)
  "Send a message to a channel. Note that you must be in the channel to send to it."
  (qsend channel 'lichat-protocol:message :channel name :text (format NIL "~{~a~^ ~}" text)))

(define-command contact (channel &rest users)
  "Contact one or more users in an anonymous channel."
  (unless users
    (error "I'll need at least one user to contact."))
  (let ((id (lichat-protocol:id (qsend channel 'lichat-protocol:create))))
    (with-awaiting (update id (main (client channel)))
      (typecase update
        (lichat-protocol:join
         (dolist (user users)
           (qsend channel 'lichat-protocol:pull
                  :channel (lichat-protocol:channel update) :target user)))
        (lichat-protocol:update-failure
         (error "Failed to create contact channel: ~a"
                (lichat-protocol:text update)))))))

(define-command p (channel)
  "Switch to the next channel in the list."
  (prev-channel (main (client channel))))

(define-command n (channel)
  "Switch to the previous channel in the list."
  (next-channel (main (client channel))))

(define-command c (channel name/index)
  "Switch to a specific channel. You can either use a name or index."
  (setf (channel (main (client channel)))
        (or (ignore-errors (max 0 (1- (parse-integer name/index))))
            name/index)))
