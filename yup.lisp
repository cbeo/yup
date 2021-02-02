;;;; yup.lisp - a lisp system for building static sites

(in-package #:yup)

(defclass site ()
  ((name :accessor site-name :initarg :name :initform "")
   (build-to :accessor build-to :initarg :build-to :initform nil)
   (assets :accessor assets :initform (make-hash-table :test 'equal)
           :documentation "Represent files that need to be copied to
           the build, and that can be embedded in various ways into
           pages.")
   (artifacts :accessor artifacts :initform (make-hash-table :test 'equal)
              :documentation "Represent objets built by YUP - pages
              created by DEFPAGE functions, scripts created by
              DEFSCRIPT functions, and stylesheets created by DEFSTYLE
              functions.")))

(defun make-site (name &key build-to)
  (make-instance 'site :name name :build-to build-to))

(defvar *site* nil
  "Used by ADD-RESOURCE, ADD-ASSET, BUILD, VIEW, and the functions defined
  with DEFPAGE")

;;; Assets

(defun add-resource (path content &optional (site *site*))
  (setf (gethash path (artifacts site)) content))

(defun get-resource (path &optional (site *site*))
  (gethash path (artifacts site)))

(defun add-asset (path filepath view &key args key)
  "VIEW is a function that accepts the FILEPATH as an argument and
produces some HTML. If ARGS are supplied these are passed to VIEW as
well.

If you wish to refer to an asset by key instead of the full path, you
can do so by supplying a KEY argument.  For example, if you want
multiple views of the same underlying asset.
"
  (assert (and (symbolp view)
               (string-equal "VIEW/" (symbol-name view) :end2 5))
          nil "A non-view passed to ADD-ASSET with path = ~a, key = ~a, filepath = ~a"
          path key filepath)
  (setf (gethash (if key key path) (assets *site*))
        (list :path path :filepath filepath :view view :args args)))

(defun get-asset (key &optional (site *site*))
  "Get an asset from a site. Returns a PLIST if the asset exists,
otherwise NIL.

KEY is the PATH identifier of the asset, or is a custom key supplied
to ADD-ASSET."
  (gethash key (assets site)))

(defun add-directory-assets
    (dir view
     &key
       (pattern uiop:*wild-file-for-directory*)
       args
       (recursive t))
  "Adds a directory on disk to the *SITE* assets.  The path of each
file will be relative to the DIR.  The same view will be given to each
file. Good for adding, e.g., directories of images or media files.

PATTERN is a PATHNAME suitable for passing to uiop:directory-files to
filter out unwanted files. 

If RECURSIVE, the process will use the same VIEW, PATTERN, and ARGS on
nested subdirectories of the initial root DIR."
  (let ((drop-prefix
          (1- (length (namestring (uiop:pathname-parent-directory-pathname dir))))))
    (directory-foreach
     dir
     (lambda (filepath)
       (apply 'add-asset
              (subseq (namestring filepath) drop-prefix)
              filepath
              view
              args))
     :pattern pattern
     :recursive recursive)))

(defun assets-like
    (pattern &key
               (case-insensitive t)
               (sort-by 'string<=))
  "Return a list of keys of assets that match the pattern."
  (let ((regex (ppcre:create-scanner pattern :case-insensitive-mode case-insensitive))
        (assets (assets *site*)))
    (sort 
     (loop :for key :being :the :hash-keys :of assets
           :when (ppcre:scan regex key) :collect key)
     sort-by)))


(defun view (path)
  "Used inside template functions, such as those defined with DEFPAGE
and DEFVIEW, to embedd an asset view into some HTML.

E.g. 

(:ul (dolist (img (assets-like \"png\")) 
        (view img)))

Where the predefined view function will be looked up and called to
produce the desired view."
  (if-let (asset (gethash path (assets *site*)))
    (apply (getf asset :view)
           (getf asset :path)
           (getf asset :args))
    (error "No asset found for ~a" path)))

;;; Pages & Templates

(defmacro defstyle (name lambda-list &body forms))

(defmacro defscript (name lambda-list &body forms))

(defmacro defpage (name (&key style js) lambda-list &body body)
  "DEFPAGE creates an html page template function that, when called,
will place an HTML string into the *PAGES* table.  When BUILD-SITE is
called, these pages will be written to disk.

(defpage moo () () ...) defines a function called PAGE/MOO

STYLE is a URL path to a stylesheet 
JS is a is a URL path to a javascript document

Whatever template-specific variables are added to the LAMBDA-LIST by
the user, DEFPAGE adds a number of its own variables. No checking is
done for unwanted anaphora.  These additional variables are:

Required Variables:

PATH will always be the first variable, it is a string.  This
represents the path of the page relative to the serving host.  The
HTML document itself will be built in a location configurable by
BUILD-SITE.

Keyword Variables:

TITLE is a string for the page title 
"
  (let* ((page-keywords '((title "") (site *site*)))
         (lambda-list (if (member '&key lambda-list
                                  :test (lambda (a b)
                                          (string-equal (symbol-name a)
                                                        (symbol-name b))))
                          (append lambda-list page-keywords)
                          (append lambda-list (cons '&key page-keywords)))))
    `(defun ,(intern (format nil "PAGE/~a" (string-upcase  name))) (path ,@lambda-list)
       (let ((*site* site))
         (add-resource
          path
          (with-html-string
            (:doctype)
            (:html
             (:head
              (:title title)
              ,(when style
                 (list :link :rel "stylesheet" :href style)))
             (:body
              ,@body
              ,(when js
                 (list :script :src js))))))))))


(defmacro deftemplate (name lambda-list &body body)
  "DEFTEMPLATE defines a simple HTML-generating function meant to be
called within a DEFPAGE body.

(deftemplate foo () ...) defines a function TEMPLATE/FOO
"
  `(defun ,(intern (format nil "TEMPLATE/~a" (string-upcase name))) ,lambda-list
     (with-html ,@body)))


(defmacro defview (name lambda-list &body body)
  "DEFVIEW defines an HTML-generating function meant to be called on
an asset pathname.  It adds ASSET to the front of the LAMBDA-LIST.

(defview blah () ...) DEFINES a function called VIEW/BLAH"
  `(defun ,(intern (format nil "VIEW/~a" (string-upcase  name))) ,(cons 'asset lambda-list)
     (assert (not (null (asset ))))
     (with-html ,@body)))

;;; Build

;;; Util

(defun directory-foreach
    (dir action
     &key
       (pattern uiop/pathname:*wild-file-for-directory*)
       (recursive t))
  "Performs an action on each file pathname in a directory.  

DIR is a directory.
ACTION is a function of one argument, a pathname. 
PATTERN is a filter for files, e.g. #P\"*.png\"
"
  (dolist (filepath (uiop:directory-files dir pattern))
    (funcall action filepath))

  (when recursive
    (dolist (subdir (uiop:subdirectories dir))
      (directory-foreach subdir action :pattern pattern :recursive t))))
