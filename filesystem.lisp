
(in-package :cl-sysop)

(defclass fs-object (component named)
  ((full-path :type string :accessor full-path :initarg :full-path)))

(defmethod destroy-plan ((x fs-object))
  `((destroy ,x)))

;; if I make changing file content use patch and diff it will make it clearer what the actual change is
;; that will be really useful for seeing what I manually hacked!
(defclass fs-file (fs-object)
  ((content :initarg :content :reader content :type string)
   ;; you can supply sha1 hash and/or content, though we won't be able to fix a missing file without the content from somewhere
   ;; without supplying either we're just asking for the file to exist
   (sha1-hash :initarg :sha1-hash :reader sha1-hash :type string)
   (permissions :initarg :permissions)
   (owner)
   (group)))

;; It might be useful to define a constructor function, but I'm not sure

(defclass fs-directory (fs-object system)
  ((subcomponents :initarg :content :reader subcomponents)))

(defmethod initialize-instance :after ((x fs-object) &rest initargs)
  (declare (ignore initargs))
  (when (and (slot-boundp x 'full-path)
             (not (slot-boundp x 'name)))
    (setf (name x)
          (first (last (cl-ppcre:split "/" (full-path x)))))))

(defmethod print-object ((x fs-object) (s stream))
  (print-unreadable-object (x s)
    (if (slot-boundp x 'full-path)
        (format s "~A ~S on host ~A"
                (class-name (class-of x))
                (full-path x)
                (host x))
        (format s "~A" (class-name (class-of x))))))

;; #'adopt
(defmethod adopt :after ((parent fs-directory) (child fs-object))
  (when (slot-boundp parent 'full-path)
    (setf (full-path child)
          (concatenate 'string
                       (full-path parent)
                       "/" (name child)))))

;; SO, setting the path in a directory updates the paths of all the children by using the adopt protocol
(defmethod (setf full-path) :after (value (dir fs-directory))
  (declare (ignore value))
  ;; now we must update the full path of our
  (dolist (c (subcomponents dir))
    (adopt dir c)))


(defun directory-list-p (list)
  (and (listp list)
       (every (lambda (x)
                (typep x 'fs-directory))
              list)))

(defmethod fs-object-exists-on-host ((f fs-object) (host localhost))
  (probe-file (full-path f)))

(defmethod exists-p ((f fs-object))
  (fs-object-exists-on-host f (host f)))



(defclass downloaded-resource (fs-file)
  ((source-url :initarg :source-url)))



(defmethod create ((x fs-directory))
  (execute-command (host x)
                   "mkdir" (list :p (full-path x))))

(defmethod create ((x fs-file))
  (execute-command (host x)
                   "cat" `((> ,(full-path x)))
                   :input (content x)))

(defmethod destroy ((x fs-file))
  (execute-command (host x)
                   "rm" (full-path x)))
