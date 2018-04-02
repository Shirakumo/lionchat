#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem lionchat
  :version "1.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A GUI for various chat systems."
  :homepage "https://github.com/Shirakumo/lionchat"
  :serial T
  :components ((:file "package")
               (:file "fa-name-map")
               (:file "consumer-widget")
               (:file "client")
               (:file "buffer")
               (:file "controller")
               (:file "toolbar")
               (:file "dockable")
               (:file "output")
               (:file "input")
               (:file "users")
               (:file "channels")
               (:file "chat"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtools
               :qtcore
               :qtgui
               :qtools-ui-listing
               :maiden
               :maiden-client-entities
               :maiden-irc
               :maiden-lichat
               :verbose)
  :build-operation "qt-program-op"
  :build-pathname "lionchat"
  :entry-point "lionchat:start")
