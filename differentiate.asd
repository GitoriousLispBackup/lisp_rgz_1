;;;; 2012-03-08 12:20:06

(defpackage #:differentiate-asd
  (:use :cl :asdf))

(in-package :differentiate-asd)

(defsystem differentiate
  :name "differentiate"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage"))
               (:file "main_with_A" :depends-on ("defpackage")))
  :depends-on ())
