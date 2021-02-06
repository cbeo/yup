;;;; yup.asd

(asdf:defsystem #:yup
  :description "A system for building static web sites."
  :author "Colin Okay <okay@toyful.space>"
  :license  "GPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:spinneret
               #:lass
               #:parenscript
               #:uiop
               #:alexandria
               #:cl-ppcre
               #:hunchentoot)
  :components ((:file "package")
               (:file "yup")))
