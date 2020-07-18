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
  "Path to a lisp file loaded during configuration")

(defgeneric configure (object conf)
  (:documentation "CONF is meant to be a PLIST and OBJECT a class
  instance. The notion is that fields from the CONF are used to configure the OBJECT.")
  (:method (object conf)))

(defgeneric build (res)
  (:documentation "A function used to transform a resource into its
  built form. Should actually output the built resource to the build destination."))

(defmacro embed (key &rest args)
  `(funcall #'embedding (gethash ,key *assets*) ,@args))

(defgeneric embedding (asset &key)
  (:documentation "A method meant to be called by the BUILD method of
  template like resources. Ordinarily called from the expanded body of
  EMBED macro."))

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


(defun default-asset-key-for (path)
  (concatenate 'string (pathname-name path) "." (pathname-type path)))


(define-condition asset-key-collision-error (error)
  ((key  :initarg :key)
   (source :initarg :source)))

(defun register-asset (asset)
  (if  (nth-value 1 (gethash (asset-key asset) *assets*))
       ;; If this exists in the *assets* table, signal an error and
       ;; handle a couple of restarts
       (restart-case (error 'asset-name-collision-error
                            :key (asset-key asset)
                            :source (source-path asset))
         (use-key (new-key)
           (setf (asset-key asset) new-key)
           (register-asset asset))
         (prompt-user ()
           (format t "The key ~a is already used by another asset.~%
             Enter a new asset key for source file at ~a~%"
                   (asset-key asset)
                   (source-path asset))
           (setf (asset-key asset) (read-line))
           (register-asset asset)))
       ;; otherwise just insert the asset into *assets*
       (setf (gethash (asset-key asset) *assets*) asset)))

(defun yupfile-p (path)
  (and (pathnamep path)
       (string-equal "yup" (pathname-type path))))

(defun load-prelude ()
  (format t "LOAD-PRELUDE not yet implemented"))

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
          '("png" "jpg" "jpeg" "mp4" "mpeg" "avi" "ogg"
            "gif" "ico" "svg" "txt" "md" "org" "ps" "parenscript" "lass")
          :test #'equal))

(defun standard-resource-file-p (path)
  (member (pathname-type path)
          '("html" "js" "css" "img" "png" "jpg" "jpeg"
            "bmp" "avi" "ogg" "mp4" "mpeg" "wav"
            "gif" "ico" "svg" "parenscript" "lass" "spinneret" "ps")
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
      (("ps" "parenscript") 'parenscript)
      ("spinneret" 'spinneret)
      (t (error "Unsupported source: ~a" (pathname-type path))))))

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
         (prompt-user ()
           (format t "The resource target ~a is already used by another resource.~%
             Enter a new resource path for source file at ~a~%"
                   (target-path resource)
                   (source-path resource))
           (setf (target-path resource) (read-line))
           (register-resource resource)))
       ;; otherwise just insert the resource into *resources*
       (setf (gethash (target-path resource) *resources*) resource)))



(defun add-source-from-config (config)
  (let ((instance (make-instance (getf config :class))))
    (configure instance config)
    (when (typep instance 'asset)
      (register-asset instance))
    (when (typep instance 'resource)
      (register-resource instance))))

(defun build-project (*source-root* *target-root*)
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
      (configureator *source-root*))
    (loop for resource being the hash-values of *resources* do (build resource))))

;;; some utilities

(defun md5sum-p (object)
  (typep object '(simple-array (unsigned-byte 8) (16))))

(defun md5sum-equal (h1 h2)
  (assert (and (md5sum-p h1) (md5sum-p h2)))
  (and (= (length h1) (length h2))
       (every #'= h1 h2)))


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


(defclass img (resource asset) ())

(defmethod embedding ((img img) &key class id width height)
  (let ((target (url-pathify-target (target-path img))))
    (spinneret:with-html
      (:img :src target :class class :id id :width width :height height))))

(defclass audio (resource asset) ())
(defclass video (resource asset) ())
(defclass txt (asset) ())

(defmethod embedding ((txt txt) &key class id)
  (let ((contents (split-sequence:split-sequence
                   #\Newline
                   (alexandria:read-file-into-string (source-path txt)))))
    (spinneret:with-html
      (dolist (content contents)
        (when (plusp (length content))
          (:p :class class :id id content))))))

(defclass markdown (asset) ())
(defmethod embedding ((md markdown) &key)
  (cl-markdown:markdown (source-path md)
                        :stream spinneret::*html*))

(defclass lass (resource asset) ())

(defmethod build ((styles lass))
  (with-slots (source target) styles
    (let ((target (ensure-path-ext target "css")))
      (ensure-directories-exist target)
      (lass:generate source :out target :pretty t))))

(defmethod embedding ((styles lass) &key)
  (with-slots (target) styles
    (let ((href (url-pathify-target target "css")))
      (spinneret:with-html
        (:link :rel "stylesheet" :type "text/css" :href href)))))


(defclass parenscript (resource asset) ())

(defmethod build ((script parenscript))
  (with-slots (source target) script
    (let ((target (ensure-path-ext target "js")))
      (ensure-directories-exist target)
      (alexandria:write-string-into-file
       (ps:ps-compile-file source)
       target
       :if-exists :supersede))))

(defmethod embedding ((script parenscript) &key)
  (with-slots (target) script
    (let ((src (url-pathify-target target "js")))
      (spinneret:with-html
        (:script :src src)))))

(defclass spinneret (resource) ())

(defmethod build ((page spinneret))
  (with-slots (source target) page
    (let ((target (ensure-path-ext target "html"))
          (markup (read-file source)))
      (ensure-directories-exist target)
      (with-open-file (spinneret:*html* target :direction :output :if-exists :supersede)
        (eval `(spinneret:with-html ,markup))))))

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
