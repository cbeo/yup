;;;; package.lisp

(defpackage #:yup
  (:use #:cl)
  (:import-from
   #:alexandria
   #:read-file-into-string
   #:write-string-into-file
   #:if-let
   #:when-let)
  (:import-from
   #:spinneret
   #:with-html
   #:with-html-string)
  (:import-from
   #:serapeum
   #:find-class-safe)
  (:export
   #:embed
   #:configure
   #:build
   #:embedding
   #:resource
   #:asset
   #:external-project
   #:img
   #:spinneret
   #:audio
   #:video
   #:lass
   #:markdown
   #:txt))

(defpackage :yup.build
  (:use :cl :yup))
