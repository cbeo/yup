;;;; yup.lisp - a lisp system for building static sites

(in-package #:yup)

(defclass site ()
  ((name
    :accessor site-name
    :initarg :name
    :initform "")
   (build-to
    :accessor build-to
    :initarg :build-to
    :initform (error "must supply a build-to location"))
   (assets
    :accessor assets
    :initform (make-hash-table :test 'equal)
    :documentation "Represent files that need to be copied to the
           build, and that can be embedded in various ways into
           pages.")
   (artifacts
    :accessor artifacts
    :initform (make-hash-table :test 'equal)
    :documentation "Represent objets built by YUP - pages created by
              DEFPAGE functions, scripts created by DEFSCRIPT
              functions, and stylesheets created by DEFSTYLE
              functions.")))

(defun make-site (name &key build-to)
  (make-instance 'site :name name :build-to build-to))

(defvar *site* nil
  "Used by ADD-RESOURCE, ADD-ASSET, VIEW, and the functions defined
  with DEFPAGE, DEFSCRIPT, and DEFSTYLE")

(defvar *auto-refresh-key* nil
  "Used by the development server started via HACK-ON. When non NIL,
  page building functions will inject a polling operation to check
  for changes and, when found, will perform a refresh of the page.")

;;; Assets

(defun add-artifact (path content &optional (site *site*))
  (setf (gethash path (artifacts site)) content))

(defun get-artifact (path &optional (site *site*))
  (gethash path (artifacts site)))

(defun add-asset (path filepath view &key args key)
  "VIEW is a function that accepts the FILEPATH as an argument and
produces some HTML. If ARGS are supplied these are passed to VIEW as
well.

If you wish to refer to an asset by key instead of the full path, you
can do so by supplying a KEY argument.  E.g. if you want multiple
views of the same underlying asset, then you can add that asset
multiple times with different views, choosing a different KEY for each
one.
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
       (pattern ".")
       args
       (recursive t))
  "Adds a directory on disk to the *SITE* assets.  The path of each
file will be relative to the DIR.  The same view will be given to each
file. Good for adding, e.g., directories of images or media files.

PATTERN is a regex pattern to match namestrings of filepaths.

If RECURSIVE, the process will use the same VIEW, PATTERN, and ARGS on
nested subdirectories of the initial root DIR."
  (let ((drop-prefix
          (1- (length (namestring (uiop:truename* (uiop:pathname-directory-pathname dir)))))))
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
               (site yup:*site*)
               (case-insensitive t)
               (sort-by 'string<=))
  "Return a list of keys of assets that match the pattern."
  (let ((regex (ppcre:create-scanner pattern :case-insensitive-mode case-insensitive))
        (assets (assets site)))
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

(defmacro defstyle (name args &body forms)
  "Defines a function that adds a stylesheet to a site. ARGS must be a
list each member of which is either a SYMBOL or a (SYMBOL VALUE) pair,
where VALUE is suitable for passing as the value of a variable in a
LASS form.

(defstyle moo () ...) defines STYLE/MOO 

The ARGS become keyword arguments to the defined function.

FORMS are the sort suitable for passint to LASS:COMPILE-AND-WRITE"
  (assert (and (listp args) (not (member '&key args)) (not (member '&rest args)))
          nil "DEFSTYLE creates implicit &KEY and &REST args - see docstring.")
  (let* ((lambda-list (append (list 'path)
                              (list* '&rest 'rest-args '&key args)
                              '((site yup:*site*))))
        (forms (mapcar (lambda (form) `(quote ,form)) forms))) 
    `(defun ,(intern (format nil "STYLE/~a" name)) ,lambda-list
       (declare (ignorable ,@(mapcar (lambda (a) (if (consp a) (car a) a))
                                     args)))
       (let ((yup:*site* site)
             (path (if (ends-with-p path ".css") path
                       (progn
                         (format *error-output* "Transforming key ~a to ~a.css" path path)
                         (concatenate 'string path ".css")))))
         (add-artifact
          path 
          (lass:compile-and-write 
           (list :let (rest-kwargs->alist
                       rest-args
                       (mapcar (lambda (entry) (if (consp entry) entry (list entry)))
                               ',args))
                 ,@forms)))))))

(defmacro defscript (name lambda-list &body forms)
  "Defines a function that adds a javascript script to a site. 

(defscript moo () ...) defines SCRIPT/MOO

FORMS are the sort suitable for passing to PARENSCRIPT:PS"
  (let ((lambda-list (if (keyword-args-p lambda-list)
                         (append (cons 'path lambda-list)
                                 '((site yup:*site*)))
                         (append (cons 'path lambda-list)
                                 '(&key (site yup:*site*)))))) 
    `(defun ,(intern (format nil "SCRIPT/~a" name)) ,lambda-list
       (let ((yup:*site* site)
             (path (if (ends-with-p path ".js") path
                       (progn
                         (format *error-output* "Transforming key ~a to ~a.js" path path)))))
         (add-artifact
          path 
          (ps:ps ,@forms))))))

(defmacro defpage (name (&key style js) lambda-list &body body)
  "Defines a template function that adds an html page to a site.  

(defpage moo () () ...) defines PAGE/MOO

Because a particular template is so closely associated with a
particular piece of JS or CSS, the macro accepts optional parameters
to include static JS ro CSS into every page created with the defined tempalte.

The BODY is a collection of forms suitable for passing to
SPINNERET:WITH-HTML-STRING."
  (let* ((page-keywords '((title "") (site yup:*site*)))
         (lambda-list (if (keyword-args-p lambda-list)
                          (append (cons 'path lambda-list) page-keywords)
                          (append (cons 'path  lambda-list) (cons '&key page-keywords)))))
    `(defun ,(intern (format nil "PAGE/~a" name)) ,lambda-list
       (let ((yup:*site* site))
         (add-artifact
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
                 (list :script :src js))
              (view/auto-refresh-script)))))))))



(defmacro defview (name lambda-list &body body)
  "Defines an HTML template function to be called from within a
function defined using DEFPAGE. 

(defview moo () ...) defines VIEW/MOO

VIEW/* functions can be associated with assets as arguments to
ADD-ASSET or ADD-DIRECTORY-ASSETS, in which case they are expected
to have a url pathname as their first argument."
  `(defun ,(intern (format nil "VIEW/~a" (string-upcase  name))) ,lambda-list
     (with-html ,@body)))


;;; UTILITY VIEWS

(defview img (src-path &key class id)
  (:img :src src-path :class class :id id))

;;; Auto Refresh Script

;; a script that is injected into defpage pages when
;; *auto-refresh-key* is non-nil.  The script polls the development
;; server for a special file containing a boolean. If the poll returns
;; true then the page refreshes.
(defview auto-refresh-script ()
  (when yup::*auto-refresh-key*
    (:script
     (ps:ps
       (let ((poll-url (+ "/" (ps:lisp yup::*auto-refresh-key*) ".json")))
         (set-interval
          (lambda ()
            (let ((fetched (fetch poll-url)))
              (ps:chain fetched
                        (then (lambda (resp) (ps:chain resp (json))))
                        (then (lambda (json)
                                (when json (ps:chain location (reload))))))))
          1000))))))

;;; Build

(defun clean (site)
  "Erases assets and artifacts from the site."
  (setf (assets site) (make-hash-table :test 'equal)
        (artifacts site) (make-hash-table :test 'equal)))


(defun build (site)
  "Builds the SITE, producing files in location indicated by the site's BUILT-TO slot."
  (let ((*site* site))
    (with-slots (name build-to assets artifacts) site
      (ensure-directories-exist build-to)
      (loop :for asset :being :the :hash-value :of assets
            :for built-loc = (make-subtree-pathname build-to (getf asset :path))
            :do
               (ensure-directories-exist built-loc)
               (uiop:copy-file
                (getf asset :filepath)
                built-loc))
      (loop :for path :being :the :hash-key :of artifacts
            :for content :being :the :hash-value :of artifacts
            :for built-loc = (make-subtree-pathname build-to path)
            :do
               (ensure-directories-exist built-loc)
               (alexandria:write-string-into-file content built-loc
                                                  :if-exists :supersede)))))

;;; Util

(defun directory-foreach
    (dir action
     &key
       (pattern ".") ; a regex
       (recursive t))
  "Performs an action on each file pathname in a directory.  

DIR is a directory.
ACTION is a function of one argument, a pathname. 
PATTERN is a regex filter for files, e.g. png$\"
"
  (dolist (filepath (uiop:directory-files dir))
    (when (ppcre:scan pattern (namestring filepath))
      (funcall action filepath)))

  (when recursive
    (dolist (subdir (uiop:subdirectories dir))
      (directory-foreach subdir action :pattern pattern :recursive t))))

(defun keyword-args-p (lambda-list)
  (member '&key lambda-list
          :test (lambda (a b)
                  (string-equal (symbol-name a)
                                (symbol-name b)))))

(defun ends-with-p (string ext)
  (string-equal string ext :start1 (- (length string) (length ext))))


(defun make-subtree-pathname (root subtree)
  (make-pathname :name (pathname-name subtree)
                 :type (pathname-type subtree)
                 :directory (append (pathname-directory root )
                                    (cdr (pathname-directory subtree))))  )

(defun rest-kwargs->alist (plist defaults)
  (loop :for (key default) :in defaults
        :for supplied = (getf plist (intern (symbol-name key) (find-package 'keyword)))
        :collect (list key
                       (if supplied supplied default))))

(defun make-auto-refresh-key ()
  (symbol-name (gensym "auto-refresh-")))

(defun backup-site (site)
  (list :assets (assets site)
        :artifacts (artifacts site)))

(defun table-subset-p (tab1 tab2 &key (test 'equal))
  "TEST compares values"
  (loop :for key :being :the :hash-key :of tab1
        :when (or (not (gethash key tab2))
                  (not (funcall test
                                (gethash key tab1)
                                (gethash key tab2))))
          :do (return nil)
        :finally (return t)))

(defun tables-equal-p (tab1 tab2 &key (test 'equal))
  (and (table-subset-p tab1 tab2 :test test)
       (table-subset-p tab2 tab1 :test test)))

(defun site-changed-p (backup site)
  "A site has changed since backed up if either the asset table or the
artifact tables have changed."
  (not (and (tables-equal-p (getf backup :assets)
                            (assets site))
            (tables-equal-p (getf backup :artifacts)
                            (artifacts site)))))

(defun mark-autorefresh-true ()
  (when (and *auto-refresh-key* *site*)
    (alexandria:write-string-into-file
     "true"
     (format nil "~a/~a.json" (build-to *site*) *auto-refresh-key*)
     :if-exists :supersede )))

(defun mark-autorefresh-false ()
  (when (and *auto-refresh-key* *site*)
    (alexandria:write-string-into-file
     "false"
     (format nil "~a/~a.json" (build-to *site*) *auto-refresh-key*)
     :if-exists :supersede )))

;;; Development Server

(defvar *development-acceptor*)

(defun hack-on (site recipe &key (port 4242) (rebuild-freqeuncy 1) auto-refresh)
  "SITE is an instance of SITE.  RECIPE is a thunk that builds the
site through calls to PAGE/*, SCRIPT/*, STYLE/*, and ADD-ASSET
functions.

It is recommend to pass in a symbol that names a thunk, that way you
can change and recompile the thunk while hacking.

The site will be built every REBUILD-FREQEUNCY seconds and served
locally on PORT."
  (setf *development-acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :port port
                                          :document-root (build-to site))))
  (bt:make-thread
   (lambda () 
     (let ((*site* site)
           (*auto-refresh-key* (when auto-refresh (make-auto-refresh-key))))
       (format t "Started Hacking on ~a~%. Connect on port ~a~%" (site-name site) port)
       (loop :while (hunchentoot:started-p  *development-acceptor*)
             :do
                (let ((backup (backup-site site)))
                  (clean site)
                  (funcall recipe) ; the recipe is likely to rely on *site* having a value
                  (cond ((site-changed-p backup site)
                         (build site)
                         (mark-autorefresh-true))
                        (*auto-refresh-key* (mark-autorefresh-false)))
                  (sleep rebuild-freqeuncy)))
       (format t "Stopped Hacking on ~a~%" (site-name site))))))


(defun stop-hacking ()
  "Stop the deveopment server."
  (hunchentoot:stop *development-acceptor*))
