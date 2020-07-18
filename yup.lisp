;;;; yup.lisp

(in-package #:yup)

;; yupfile contains :source :destination :asset and :class

(defvar *source-root* nil)
(defvar *target-root* nil)
(defvar *assets* nil
  "Maps string keys to ASSET instances")

(defvar *resources* nil
  "Maps target paths to instances of RESOURCE")

(defvar *prelude* nil
  "Path to a lisp file loaded during configuration. If NIL a default
  prelude , called 'prelude.lisp' will be searched for in
  *SOURCE-ROOT*")

(defvar *configuration-restart-policy* nil
  "Either :INTERACTIVE or NIL.  

If :INTERACTIVE, the user will be prompted to resolve configuration
errors as they come up.

If NIL, if configuration errors are encountered, they will be printed
to the terminal and the project will not be built.")

(defvar *config-error-log* nil
  "The BUILD-PROJECT will not actually build the project if errors are
  encountered and logged to this list.  Instead, the log will be
  printed and you will be asked to resolve problems.")

(defgeneric configure (object conf)
  (:documentation "CONF is meant to be a PLIST and OBJECT a class
  instance. The notion is that fields from the CONF are used to configure the OBJECT.")
  (:method (object conf)))

(defgeneric build (res)
  (:documentation "A function used to transform a resource into its
  built form. Should actually output the built resource to the build destination."))

(defvar *building-resource* nil
  "Dynamically bound  by BUILD - for use in EMBEDDING implementations.")

(defmacro embed (key &rest args)
  `(funcall #'embedding *building-resource* (gethash ,key *assets*) ,@args))

(defgeneric embedding (resource asset &key)
  (:documentation "Embeds the ASSET in the RESOURCE.  If evoked from
  an expansion of the EMBED macro, then resource will be the current
  value of *BUILDING-RESOURCE*, which will have been bound in the call
  to BUILD."))

(defclass source ()
  ((source
    :accessor source-path
    :initarg :source
    :initform nil
    :type pathname
    :documentation "This is the location, on disk, where the source
    file is located.")  ))

(defmethod configure :after ((ob source) conf)
  (when-let (path (getf conf :source))
    (when (and (uiop:file-pathname-p path)
               (uiop:file-exists-p path))
      (setf (source-path ob) path))))

(defclass resource (source)
  ((target
     :accessor target-path
     :initarg :target-path
     :initform nil
     :documentation "A directory (or some other location) where the
     built resorce will be placed.")))

(defmethod configure :after ((ob resource) conf)
  (when-let (dest (getf conf :destination))
    (when (uiop:file-pathname-p dest)
      (setf (target-path ob) dest))))

(defmethod build :around ((res resource))
  (restart-case 
      (let ((*building-resource* res)) 
        (call-next-method))
    (print-and-retry (e)
      (format t "While building ~a~%an error was encountered:~%~a~%"
              (source-path res) e)
      (if (y-or-n-p "Is the file ready to try rebuilding?")
          (build res)
          (format "Skipping ~a~%" (source-path res))))
    (print-and-skip (e)
      (format t "While building ~a~%an error was encountered:~%~a~%"
              (source-path res) e)
      (format "Skipping ~a~%" (source-path res)))))

(defmethod build ((res resource))
  (with-slots (source target) res 
    (ensure-directories-exist target)
    (uiop:copy-file source target)))

(defclass asset (source)
  ((key
    :accessor asset-key
    :initarg :key
    :initform nil
    :type (or string symbol)
    :documentation "Asset keys are used to refer to embeddable assets
    from within templates.  Upon initialization, assets are registered
    in a global asset table keyed by this KEY.")))

(defmethod configure :after ((ob asset) conf)
  (if-let (key (getf conf :asset))
    (setf (asset-key ob) key)
    (setf (asset-key ob)
          (default-asset-key-for (source-path ob)))))


(defun default-asset-key-for (path &optional include-type)
  (concatenate 'string (pathname-name path)
               (when include-type (list  "." (pathname-type path)))))

(define-condition asset-key-collision-error (error)
  ((key  :initarg :key)
   (source :initarg :source)))

(defmethod print-object ((c asset-key-collision-error) s)
  (with-slots (key source) c
    (format s "KEY COLLISION: The asset key ~s is already in use by another asset.~%   Source: ~a~%"
            key source)
    c))

(defun prompt-for-key (c)
  (when-let (restart (find-restart 'use-key))
    (princ c) (terpri)
    (princ "You must choose a new a new asset key. Before entering the key, however, be sure") (terpri)
    (princ "to update any source files that may embed this asset to reflect the new key.") (terpri)
    (princ "NEW KEY: ") (force-output)
    (invoke-restart restart (read-line))))

(defun log-key-collision (c)
  (when-let (restart (find-restart 'skip-asset))
    (push c *config-error-log*)
    (invoke-restart restart)))

(defun register-asset (asset)
  (if  (nth-value 1 (gethash (asset-key asset) *assets*))
       ;; If this exists in the *assets* table, signal an error and
       ;; handle a couple of restarts
       (restart-case (error 'asset-key-collision-error
                            :key (asset-key asset)
                            :source (source-path asset))
         (use-key (new-key)
           (setf (asset-key asset) new-key)
           (register-asset asset))

         (skip-asset () (return-from register-asset)))
       ;; otherwise just insert the asset into *assets*
       (setf (gethash (asset-key asset) *assets*) asset)))

(defun yupfile-p (path)
  (and (pathnamep path)
       (string-equal "yup" (pathname-type path))))

(defun default-prelude-file ()
  (or (and *prelude* (pathname *prelude*))
      (cl-fad:merge-pathnames-as-file (uiop:getcwd) "prelude.lisp")))

(defun load-prelude ()
  (let ((prelude-file (default-prelude-file)))
    (when (uiop:file-exists-p prelude-file)
      (load prelude-file))))

(defun read-file (path)
  (with-open-file (input path)
    (read input)))


;; TODO: make this portable
(defun strip-path-root (path root)
  (let ((root (namestring root))
        (path (namestring path)))
    (if (string= root path :end2 (length root))
        (pathname (subseq path (length root)))
        (error "~a is not a prefix path of ~a~%" root path))))

;; TODO: make this portable 
(defun path-append (&rest paths)
  (pathname (apply #'concatenate
                   'string
                   (loop :for (p . more) :on paths
                      :when more :appending (list (namestring p) "/")
                      :else :appending (list (namestring p))))))

(defun standard-asset-file-p (path)
  (member (pathname-type path)
          '("png" "jpg" "jpeg" "mp4" "webm" "avi" "ogg" "wav" "mp3"
            "gif" "ico" "svg" "txt" "md" "parenscript" "lass")
          :test #'equal))

(defun standard-resource-file-p (path)
  (member (pathname-type path)
          '("html" "js" "css" "img" "png" "jpg" "jpeg"
            "bmp" "avi" "ogg" "mp4" "mpeg" "wav" "webm" "mp3"
            "gif" "ico" "svg" "parenscript" "lass" "spinneret")
          :test #'equal))

(defmacro string-case (exp &body body)
  (let* ((val (gensym))
         (clauses (mapcar (lambda (clause)
                            (cond ((consp (car clause))
                                   (list* `(member ,val ',(car clause) :test #'equal)
                                          (cdr clause)))
                                  ((stringp (car clause))
                                   (list* `(equal ,val ,(car clause))
                                          (cdr clause)))
                                  ((eq t (car clause)) clause)
                                  (t (error "malformed string case clause: ~a" clause))))
                          body)))
    `(let ((,val ,exp))
       (cond ,@clauses))))

;; this whole function should eventually be rewritten to be extensible
;; ile. by using some dynamically bound dictionary.
(defun default-source-class-for (path)
  (unless (uiop:directory-pathname-p path)
    (string-case (pathname-type path)
      (("js" "html" "css") 'resource)
      (("mp3" "ogg" "wav") 'audio)        ; audio and video are probably wrong
      (("mp4" "webm") 'video)             ; probably wrongly thought....
      (("png" "bmp" "gif" "jpg" "jpeg" "ico") 'img) ;should support more image types
      ("txt" 'txt)                        
      ("md" 'markdown)
      ("lass" 'lass)
      ("parenscript" 'parenscript)
      ("spinneret" 'spinneret)
      (t nil))))

(defun guess-yup-from-path (path)
  (unless (uiop:directory-pathname-p path)
    (append (list :source path :path path) ;; path is "yup config" source is "file source"
            (when (standard-resource-file-p path)
              (list :destination
                    (path-append
                     *target-root*
                     (strip-path-root path *source-root*))))
            (when (standard-asset-file-p path)
              (list :asset (default-asset-key-for path)))
            (list :class (default-source-class-for path)))))

(defun update-plist (orig &rest update-lists)
  (dolist (updates update-lists)
    (loop :for (key val . _) :on updates :by #'cddr
       :do (setf (getf orig key) val)))
  orig)

(defun read-yupfile (yupfile)
  (list* :path yupfile (read-file yupfile)))

(defun process-source-config (path yupfile directory-overrides)
  "Either PATH or YUPFILE may be NIL, DIRECTORY-OVERRIDES is a PLIST
that may also be NIL.

Returns a complete config"
  (cond ((and path yupfile)  
         (update-plist (guess-yup-from-path path)
                       directory-overrides
                       (read-yupfile yupfile))) ; TODO put this into a defun
        (path
         (update-plist (guess-yup-from-path path)
                       directory-overrides))

        (yupfile (read-yupfile yupfile))

        (t (error "Cannot configure null path with null yupfile"))))

;; TODO: try tomake this portable
(defun actually-hidden-pathname-p (path)
  "Takes a path, any path, and returns T if it is a hidden path on
UNIXs systems.

UIOP:HIDDEN-PATHNAME-P doesn't return T on hidden directories."
  (or (uiop:hidden-pathname-p path)
      (and (uiop:directory-pathname-p path)
           (eql #\.
                (elt (first (last (pathname-directory path)))
                     0)))))

(defun directory-config-scan (directory)
  "Scans the DIRECTORY for files, subdirectories, and yupfiles (ending
in .yup).

Returns a list of pairs (PATH . YUPFILE). PATH and YUPFILE are both
either a path name or are NIL."
  (let* ((files (remove-if #'actually-hidden-pathname-p (uiop:directory-files directory)))
         (yup-files (remove-if-not #'yupfile-p files))
         (regular-files (remove-if #'yupfile-p files))
         (config)
         (mapping))

    (flet ((file-yupfile (file)
             (car (member (pathname-name file) yup-files
                          :key #'pathname-name :test #'equal)))

           (directory-config-file-p (file)
             (and (yupfile-p file)
                  (equal "_here_" (pathname-name file)))))

      (dolist (file regular-files)
        (let ((yf (file-yupfile file)))
          (when yf (setf yup-files (delete yf yup-files :test #'equal)))
          (push (cons file yf) mapping)))

      (setf config (car (remove-if-not #'directory-config-file-p yup-files)))

      (when config
        (setf yup-files (remove-if #'directory-config-file-p yup-files)))

      (values config
              (nconc mapping (mapcar (lambda (yf) (cons nil yf)) yup-files))))))

(define-condition resource-target-collision-error (error)
  ((source :initarg :source)
   (target :initarg :target)))

(defmethod print-object ((c resource-target-collision-error) s)
  (with-slots (source target) c
    (format s "TARGET COLLISION: The build target ~s is used by another resource.~%  Source: ~s~%"
            target source)
    c))

(defun prompt-for-target (c)
  (when-let (restart (find-restart 'use-target))
    (princ c) (terpri)
    (princ "You should choose a new target path. Before you do, however, ensure") (terpri)
    (princ "that you update your source files to reflect the new target.~%") (terpri)
    (princ "NEW TARGET: ") (force-output)
    (invoke-restart restart (read-line))))

(defun log-target-collision (c)
  (when-let (restart (find-restart 'skip-resource))
    (push c *config-error-log*)
    (invoke-restart restart)))

(defun register-resource (resource)
  (if  (nth-value 1 (gethash (target-path resource) *resources*))
       ;; If this exists in the *resources* table, signal an error and
       ;; handle a couple of restarts
       (restart-case (error 'resource-target-collision-error
                            :source (source-path resource)
                            :target (target-path resource))
         (use-target (new-target)
           (setf (target-path resource) new-target)
           (register-resource resource))

         (skip-resource () (return-from register-resource)))
       ;; otherwise just insert the resource into *resources*
       (setf (gethash (target-path resource) *resources*) resource)))

(define-condition no-such-source-class (error)
  ((class :initarg :class)
   (source :initarg :source)))

(defmethod print-object ((c no-such-source-class) s)
  (with-slots (class source) c
    (format s "BAD SOURCE CLASS: The configuration for ~s~%is trying to use a class ~s, but no such class is found.~%"
     source class)))

(defun find-class-from-config (config)
  (if-let (class (find-class-safe (getf config :class)))
    class
    (error 'no-such-source-class
           :class (getf config :class)
           :source (getf config :source))))

(defun prompt-for-class (c)
  (when-let (restart (find-restart 'use-class-named))
    (princ c) (terpri)
    (princ "Choose a new class: ")
    (invoke-restart restart (read))))

(defun add-source-from-config (config)
  (restart-case 
      (let ((instance (make-instance (find-class-from-config config))))
        (configure instance config)
        (when (typep instance 'asset)
          (register-asset instance))
        (when (typep instance 'resource)
          (register-resource instance)))
    (use-class-named (class-name)
      (setf (getf config :class) class-name)
      (add-source-from-config config))
    (skip-config () (return-from add-source-from-config))))

;;; the main project builder

(defun select-asset-collision-handler-policy ()
  (case *configuration-restart-policy*
    (:interactive #'prompt-for-key)
    (t #'log-key-collision)))

(defun select-resource-collision-handler-policy ()
  (case *configuration-restart-policy*
    (:interactive #'prompt-for-target)
    (t #'log-target-collision)))

(defun prompt-and-retry (e)
  (when-let (restart (find-restart 'print-and-retry))
    (invoke-restart restart e)))

(defun select-build-error-policy ()
  (case *configuration-restart-policy*
    (:interactive #'prompt-and-retry)
    (t (lambda (e) (when-let (restart (find-restart 'print-and-skip))
                     (invoke-restart restart e))))))

(defun select-no-such-source-class-handler-policy ()
  (case *configuration-restart-policy*
    (:interactive #'prompt-for-class)
    (t (lambda (e) (when-let (restart (find-restart 'skip-config))
                     (push e *config-error-log*)
                     (invoke-restart restart))))))


(defun build-project ()
  "Builds a project. Relies heavily on the values of various special
variables, which must be set before build-project is run: 

- *SOURCE-ROOT* 
- *TARGET-ROOT* 
- *PRELUDE* 
- *CONFIG-ERROR-LOG* 
- *CONFIGURATION-RESTART-POLICY* 

See docstrings for each for further information.
"

  (load-prelude)
  (let ((*assets* (make-hash-table :test 'equal))
        (*resources* (make-hash-table :test 'equal)))

    (labels ((configureator (dir &key parent-overrides)
               (multiple-value-bind (config mapping) (directory-config-scan dir)
                 (let ((overrides (if config
                                      (update-plist parent-overrides (read-file config))
                                      parent-overrides)))
                   (loop :for (src . yup) :in mapping
                      :do (add-source-from-config
                           (process-source-config src yup overrides)))
                   (dolist (subdir (uiop:subdirectories dir))
                     (configureator subdir :parent-overrides overrides))))))

      (handler-bind ((asset-key-collision-error (select-asset-collision-handler-policy))
                     (resource-target-collision-error (select-resource-collision-handler-policy))
                     (no-such-source-class (select-no-such-source-class-handler-policy)))
        (format t "~%CONFIGURING THE BUILD ...~%~%")
        (configureator *source-root*)))

    (cond ((null *config-error-log*)
           (format t "~~% ... CONFIGURATION OK!~%~% BUILDING ...~%~%")
           (handler-bind ((error (select-build-error-policy)))
             (loop
                :for resource :being :the :hash-values :of *resources*
                :do (build resource))))
          (t 
           (print-config-error-report)))))


(defun print-config-error-report ()
  (format t "~%The project was not built due to the presence of these configuration errors:~%~%")
  (dolist (e *config-error-log*)
    (format t "- ~a~%" e)))

;;; some utilities

(defun ensure-path-ext (path ext)
  (let ((path (namestring path)))
    (pathname
     (concatenate 'string
                  (subseq path 0 (- (length path)
                                    (length (pathname-type path))))
                  ext))))


(defun url-pathify-target (build-target-path &optional ext)
  (let ((newpath (cl-fad:merge-pathnames-as-file
                  "/"
                  (strip-path-root build-target-path *target-root*))))
    (if ext (ensure-path-ext newpath ext)
        newpath)))

;;; some classes

(defclass spinneret (resource asset) ())

(defmethod embedding ((res spinneret) (page spinneret)
                      &key (link-text (pathname-name (target-path page))) class id)
  (with-html
    (:a :href (url-pathify-target (target-path page) "html")
        :class class :id id
        link-text)))

(defmethod build ((page spinneret))
  (with-slots (source target) page
    (let ((target (ensure-path-ext target "html"))
          (markup (read-file source)))
      (ensure-directories-exist target)
      (with-open-file (spinneret:*html* target :direction :output :if-exists :supersede)
        (eval `(spinneret:with-html ,markup))))))

(defclass img (resource asset) ())

(defmethod embedding ((res spinneret) (img img) &key class id width height)
  (let ((target (url-pathify-target (target-path img))))
    (with-html
      (:img :src target :class class :id id :width width :height height))))

(defclass audio (resource asset) ())
(defclass video (resource asset) ())

(defclass txt (asset) ())

(defmethod embedding ((res spinneret) (txt txt) &key class id)
  (let ((contents (split-sequence:split-sequence
                   #\Newline
                   (read-file-into-string (source-path txt)))))
    (with-html
      (dolist (content contents)
        (when (plusp (length content))
          (:p :class class :id id content))))))

(defclass markdown (asset) ())
(defmethod embedding ((res spinneret) (md markdown) &key)
  (cl-markdown:markdown (source-path md)
                        :stream spinneret::*html*))

(defclass lass (resource asset) ())

(defmethod build ((styles lass))
  (with-slots (source target) styles
    (let ((target (ensure-path-ext target "css")))
      (ensure-directories-exist target)
      (lass:generate source :out target :pretty t))))

(defmethod embedding ((res spinneret) (styles lass) &key)
  (with-slots (target) styles
    (let ((href (url-pathify-target target "css")))
      (with-html
        (:link :rel "stylesheet" :type "text/css" :href href)))))


(defclass parenscript (resource asset) ())

(defmethod build ((script parenscript))
  (with-slots (source target) script
    (let ((target (ensure-path-ext target "js")))
      (ensure-directories-exist target)
      (write-string-into-file
       (ps:ps-compile-file source)
       target
       :if-exists :supersede))))

(defmethod embedding ((res spinneret) (script parenscript) &key)
  (with-slots (target) script
    (let ((src (url-pathify-target target "js")))
      (with-html
        (:script :src src)))))


(defclass external-project (resource) ()
  (:documentation "SOURCE represents a directory outside of the scope
  of the *SOURCE-ROOT* Instances of EXTERNAL-PROJECT are a way to
  include other document roots into your project. E.g. If you have a
  static site in another git repo on your machine, you can include an
  external-project config in your source tree and it will be copied to
  your build directory."))

(defmethod build ((proj external-project))
  (with-slots (source target) proj
    (assert (uiop:directory-pathname-p source))
    (assert (uiop:directory-pathname-p target))
    (uiop:collect-sub*directories
     source
     (complement #'actually-hidden-pathname-p)
     (complement #'actually-hidden-pathname-p)
     (lambda (dir)
       (dolist (file (uiop:directory-files dir))
         (unless (actually-hidden-pathname-p file)
           (let ((file-target (path-append target (strip-path-root file source))))
             (ensure-directories-exist file-target)
             (uiop:copy-file file file-target))))))))
