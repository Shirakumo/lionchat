#|
 This file is a part of lionchat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem lionchat
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A GUI client for the Lichat protocol"
  :homepage "https://github.com/Shinmera/lionchat"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "connect")
               (:file "updatable")
               (:file "client")
               (:file "user")
               (:file "channel")
               (:file "channel-list")
               (:file "chat-area")
               (:file "main"))
  :depends-on (:lichat-tcp-client
               :qtools-ui-listing
               :qtools
               :qtcore
               :qtgui
               :bordeaux-threads
               :documentation-utils
               :verbose
               :cl-ppcre))
