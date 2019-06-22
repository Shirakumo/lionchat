#|
 This file is a part of lionchat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem lionchat
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A GUI client for the Lichat protocol"
  :homepage "https://Shinmera.github.io/lionchat/"
  :bug-tracker "https://github.com/Shinmera/lionchat/issues"
  :source-control (:git "https://github.com/Shinmera/lionchat.git")
  :build-operation "qt-program-op"
  :build-pathname "lionchat"
  :entry-point "lionchat:start"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "settings")
               (:file "updatable")
               (:file "client")
               (:file "user")
               (:file "user-list")
               (:file "channel")
               (:file "channel-list")
               (:file "commands")
               (:file "chat-area")
               (:file "repl")
               (:file "tray")
               (:file "main")
               (:file "shortcut"))
  :defsystem-depends-on (:qtools)
  :depends-on (:lichat-tcp-client
               :qtools
               :qtcore
               :qtgui
               :qtsvg
               :qtools-ui-listing
               :qtools-ui-options
               :qtools-ui-notification
               :qtools-ui-repl
               :alexandria
               :bordeaux-threads
               :documentation-utils
               :verbose
               :cl-ppcre
               :ubiquitous
               :trivial-arguments))
