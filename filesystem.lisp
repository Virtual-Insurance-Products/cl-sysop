
(in-package :cl-sysop)

(defclass fs-object (component named)
  ((full-path :type string :accessor full-path :initarg :full-path)
   (existing-content :reader existing-content)))


;; if I make changing file content use patch and diff it will make it clearer what the actual change is
;; that will be really useful for seeing what I manually hacked!
(defclass fs-file (fs-object)
  ((content :initarg :content :type (or string function))
   ;; you can supply sha1 hash and/or content, though we won't be able to fix a missing file without the content from somewhere
   ;; without supplying either we're just asking for the file to exist
   (sha1-hash :initarg :sha1-hash :reader sha1-hash :type string)
   (permissions :initarg :permissions :reader permissions)
   (owner)
   (group)))

(defmethod existing-content :before ((file fs-file))
  (unless (slot-boundp file 'existing-content)
    (when (exists-p file)
      (setf (slot-value file 'existing-content)
            (execute-command (host file)
                             "cat"
                             (full-path file))))))

(defmethod content ((x fs-file))
  (if (functionp (slot-value x 'content))
      (funcall (slot-value x 'content))
      (slot-value x 'content)))

;; It might be useful to define a constructor function, but I'm not sure

;; we can just replace it
;; I'm only giving this op a different name to distinguish it
(defmethod update-file-content ((file fs-file))
  (create file))

(defmethod create-plan ((file fs-file))
  (if (slot-boundp file 'permissions)
      (append (call-next-method)
              `((chmod ,file)))
      (call-next-method)))

(defmethod update-plan ((file fs-file) &optional without)
  (if without
      (call-next-method)
      (append (call-next-method)
              (when (slot-boundp file 'content)
                (unless (equal (content file)
                               (existing-content file))
                  `((update-file-content ,file)))))))

(defmethod diff ((file fs-file))
  (diff:render-diff (diff:generate-seq-diff 'diff:unified-diff ; or diff:context-diff
                                            (cl-ppcre:split "\\n" (existing-content file))
                                            (cl-ppcre:split "\\n" (content file)))
                    *standard-output*))


(defclass fs-directory (system fs-object)
  ((subcomponents :initarg :content :reader subcomponents)
   ;; if this is set then we will empty any OTHER content of the directory
   (exclude-others :initform nil :initarg :exclude-others :reader exclude-others)))

(defmethod initialize-instance :after ((x fs-object) &rest initargs)
  (declare (ignore initargs))
  (when (and (slot-boundp x 'full-path)
             (not (slot-boundp x 'name)))
    (setf (name x)
          (first (last (cl-ppcre:split "/" (full-path x)))))))

;; This is needed as otherwise the system #'exists-p will be called which assumes T
(defmethod exists-p ((dir fs-directory))
  (fs-object-exists-on-host dir (host dir)))

(defmethod update-plan :before ((dir fs-directory) &optional without)
  (declare (ignore without))
  (slot-makunbound dir 'existing-content))

(defmethod existing-content :before ((dir fs-directory))
  (unless (slot-boundp dir 'existing-content)
    (when (exists-p dir)
      (setf (slot-value dir 'existing-content)
            (loop for line in (setf (slot-value dir 'existing-content)
                                    (execute-command (host dir)
                                                     "ls"
                                                     (list :p (concatenate 'string
                                                                           (full-path dir) "/"))
                                                     :output :lines))
                  collect (cond ((cl-ppcre:scan "/$" line)
                                 (adopt dir (make-instance 'fs-directory :name (subseq line 0
                                                                                       (1- (length line)))
                                                                         :content nil)))
                                (t (adopt dir (make-instance 'fs-file :name line))))))
      )))

;; Is a the same component as b?
(defmethod component-p ((a fs-object) (b fs-object))
  (and (equal (full-path a)
              (full-path b))
       (subtypep (type-of a)
                 (type-of b))))

(defmethod update-plan ((dir fs-directory) &optional without)
  (unless without
    (if (exclude-others dir)
        (let ((existing (existing-content dir)))
          (append (reduce #'append
                          (loop for f in existing
                                unless (find f (subcomponents dir) :test #'component-p)
                                  collect (destroy-plan f)))
                  (call-next-method)))
        (call-next-method))))

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

(defmethod fs-object-exists-on-host ((f fs-object) (host unix-host))
  (handler-case
      (execute-command host "ls" (list "-l" (full-path f)))
    (shell-error (c)
      (when (cl-ppcre:scan "no such file" (standard-error c))
        nil))))


(defclass downloaded-resource (fs-file)
  ((source-url :initarg :source-url)))



(defmethod create ((x fs-directory))
  (execute-command (host x)
                   "mkdir" (list :p (full-path x))))

(defmethod create ((x fs-file))
  (execute-command (host x)
                   "cat" `((> ,(full-path x)))
                   :input (content x)))

(defmethod append-file ((x fs-file))
  (execute-command (host x)
                   "cat" `((>> ,(full-path x)))
                   :input (content x)))

(defmethod chmod ((x fs-file))
  (execute-command (host x)
                   "chmod" (list (format nil "~O" (permissions x))
                                 (full-path x))))

(defmethod destroy ((x fs-file))
  (execute-command (host x)
                   "rm" (full-path x)))

(defmethod destroy ((x fs-directory))
  (execute-command (host x)
                   "rm"
                   (list "-rf" (full-path x))))


(defmethod tail ((x fs-file) &key (lines 10) (output :string))
  (execute-command (host x)
                   "tail"
                   (list :n lines (full-path x))
                   :output output))




(defun temporary-file (content &optional (host (make-instance 'localhost :name "localhost")))
  (create (adopt host
                 (make-instance 'fs-file :content content
                                         :full-path (concatenate 'string "/tmp/"
                                                                 (vip-utils:random-string 20))))))

(defun temporary-directory (content &optional (host (make-instance 'localhost :name "localhost")))
  (let* ((dir (adopt host
                     (make-instance 'fs-directory :content content
                                                  :full-path (concatenate 'string "/tmp/"
                                                                          (vip-utils:random-string 20)))))
         (plan (create-plan dir)))
    ;; This will show lots of debug output
    (execute-plan plan)
    dir))

(defmacro in-temporary-directory (content &body forms)
  (let ((dir (gensym "dir"))
        (prev (gensym "prev")))
    `(with-temporary-resources
         ((,dir (temporary-directory ,content)))
       (let ((,prev (ccl:current-directory)))
         (ccl:cwd (full-path ,dir))
         (unwind-protect
              (progn
                ,@forms)
           (ccl:cwd ,prev))))))




(defclass downloaded-file (fs-file)
  ((source-url :initarg :source-url :reader source-url)))

