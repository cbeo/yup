;;;; package.lisp

(defpackage #:yup
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:import-from #:spinneret
                #:with-html
                #:with-html-string)
  (:export
   #:*site*
   #:add-artifact
   #:add-asset
   #:add-directory-assets
   #:add-page
   #:add-pre-build-action
   #:add-post-build-action
   #:assets-like
   #:build
   #:build-to
   #:clean
   #:defpage
   #:defview
   #:defstyle
   #:defscript
   #:directory-foreach
   #:get-asset
   #:hack-on
   #:make-site
   #:site-name
   #:stop-hacking
   #:view
   #:view/img))


