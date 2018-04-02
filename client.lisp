#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(defclass client (maiden:client)
  ())

(maiden:define-consumer lichat-client (maiden-lichat:lichat-client)
  ())

(defmethod settings ((client (eql 'lichat-client)))
  '((:host text :default "chat.tymoon.eu")
    (:port number :default 1111 :range (1 . 65535))
    (:username text :default "Lion" :range (1 . 32))
    (:password password)))

(maiden:define-consumer irc-client (maiden-irc:irc-client)
  ())

(defmethod settings ((client (eql 'irc-client)))
  '((:hostname text :default "irc.tymoon.eu")
    (:port number :default 6667 :range (1 . 65535))
    (:username text :default (machine-instance) :range (1 . 30))
    (:password password)
    (:realname text :default (machine-instance) :range (1 . 30))
    (:nickname text :default "Lion" :range (1 . 30))
    (:services-password password)))
