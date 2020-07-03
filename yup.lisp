;;;; yup.lisp

(in-package #:yup)

;; yupfile contains a single PLIST. The PLIST has zero or all of the
;; following keys: :DIRECTORY-P, :SOURCE, :DESTINATION, :ASSET-KEY,
;; :EMBED-WITH, :BUILD-WITH.  not all keys make sense for all files.

;; yupfile contains :source :destination :asset

(defvar *project-root*)
(defvar *assets* (make-hash-table :test 'equal))

(defclass source ()
  ((parent
    :accessor parent
    :initarg :parent
    :initform *root-dir*
    :type pathname
    :documentation "The logical parent directory of this entry. Some
    entries may have a source anywhere, but for configuration
    purposes, are considered to be located within the project
    tree. This field places them within the tree. If no parent is
    supplied, the dyanmically bound value of *PROJECT-ROOT* is used.")
   (source
    :reader source
    :initarg :source
    :initform (error "all source instances must have a source")
    :type pathname
    :documentation "This is the location, on disk, where the source
    file is located.")  ))

(defclass resource (source)
  ((build-destination
     :accessor build-destination
     :initarg :build-destination
     :initform (error "all resources must have a build-destination")
     :documentation "A directory (or some other location) where the
     built resorce will be placed.")))

(defgeneric build (res)
  (:documentation "A function used to transform a resource into its
  built form. Should actually output the built resource to the build destination."))

(defmethod build ((res resource))
  (with-slots (source build-destination) res 
    (uiop:copy-file source build-destination)))

(defclass asset (source)
  ((key
    :accessor asset-key
    :initarg :key
    :initform nil
    :type (or string symbol)
    :documentation "Asset keys are used to refer to embeddable assets
    from within templates.  Upon initialization, assets are registered
    in a global asset table keyed by this KEY.")))

(defun default-asset-key-for (path)
  (concatenate 'string (pathname-name path) "." (pathname-type path)))

(defmethod initialize-instance :after ((asset asset) &key)
  (unless (asset-key asset)
    (setf (asset-key asset) (default-asset-key-for (source asset))))
  (register-asset asset))

(defun register-asset (asset)
  (setf (gethash (asset-key asset) *assets*) asset))

(defun yupfile-p (path)
  (and (pathnamep path)
       (string-equal "yup" (pathname-type path))))

(defun load-prelude (directory)
  (format t "LOAD-PRELUDE not yet implemented"))

(defun configure-path (path yupfile)
  "Either PATH or YUPFILE may be NIL"
  (cond ((and path yupfile)
         )))

(defun actually-hidden-pathname-p (path)
  "Takes a path, any path, and returns T if it is a hidden path on
UNIXs systems.

Uiop:HIDDEN-PATHNAME-P doesn't return T on hidden directories."
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
         (subdirs (remove-if #'actually-hidden-pathname-p (uiop:subdirectories directory)))
         (mapping))
    (flet ((file-yupfile (file)
             (car (member (pathname-name file) yup-files
                          :key #'pathname-name :test #'equal)))
           (dir-yupfile (dir)
             (car (member (first (last (pathname-directory dir)))
                          yup-files
                          :key #'pathname-name :test #'equal))))
      (dolist (file regular-files)
        (let ((yf (file-yupfile file)))
          (when yf (setf yup-files (delete yf yup-files :test #'equal)))
          (push (cons file yf) mapping)))
      (dolist (dir subdirs)
        (let ((yf (dir-yupfile dir)))
          (when yf (setf yup-files (delete yf yup-files :test #'equal)))
          (push (cons dir yf) mapping)))
      (nconc mapping (mapcar (lambda (yf) (cons nil yf)) yup-files)))))



(defun md5sum-p (object)
  (typep object '(simple-array (unsigned-byte 8) (16))))

(defun md5sum-equal (h1 h2)
  (assert (and (md5sum-p h1) (md5sum-p h2)))
  (and (= (length h1) (length h2))
       (every #'= h1 h2)))
