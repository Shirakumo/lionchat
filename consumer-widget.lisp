#|
 This file is a part of Lionchat
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lionchat)
(in-readtable :qtools)

(defclass consumer-widget-class (widget-class maiden:consumer-class)
  ())

(defclass consumer-widget (widget maiden:consumer)
  ()
  (:metaclass consumer-widget-class)
  (:qt-superclass "QObject"))

(defmacro define-consumer-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'consumer-widget)))
    (push 'consumer-widget direct-superclasses))
  (pushnew `(:metaclass consumer-widget-class) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  (pushnew `(:qt-superclass ,(eqt-class-name qt-class)) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  ;; Ensure compile-time test
  (qtools::check-qt-superclass-compatibility qt-class direct-superclasses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
