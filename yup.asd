;;;; yup.asd

(asdf:defsystem #:yup
  :description "Describe yup here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:spinneret #:lass #:parenscript #:cl-conspack #:uiop #:md5 #:cl-markdown)
  :components ((:file "package")
               (:file "yup")))
