
(in-package :cl-sysop)

;; These are OS software packages - not the CL packages

;; I think I should make hosts which have associated package managers

(defclass software-package (component named)
  ((version :initarg :version :type (or string (eql :latest)) :reader version)
   (minimum-version :initarg :minimum-version :reader minimum-version)))

;; now we'll have subclasses
(defclass pkgin-package (software-package)
  ((description :initarg :description)))

(defclass brew-package (software-package)
  ())


(defclass brew-host ()
  ;; I'll assume it's installed
  ((brew-installed-p :accessor brew-installed-p :initform t :initarg :brew-installed-p)))

(defclass pkgin-host ()
  ((pkgin-installed-p :accessor pkgin-installed-p :initform t :initarg :pkgin-installed-p)))

;; then we can do this...
;; this only changes the generic (abstract) type
;; so if you want a pkgin package you can still have one
(defmethod adopt :after ((parent darwin-host) (child software-package))
  (when (eq 'software-package (type-of child))
    (change-class child 'brew-package)))

(defmethod adopt :after ((parent pkgin-host) (child software-package))
  (when (eq 'software-package (type-of child))
    (change-class child 'pkgin-package)))


;; examine state of installed packages on host
;; we will cache the whole package list on the host whilst we do this sort of thing

;; Base method so I can append
(defmethod retrieve-installed-packages ((host unix-host)) nil)

;; we could have pkgin installed as well so we should query that
;; it would be good to get versions. Maybe there's an option
(defmethod retrieve-installed-packages ((host brew-host))
  (append (call-next-method)
          (when (brew-installed-p host)
            ;; See also 'brew leaves'
            ;; https://apple.stackexchange.com/questions/101090/list-of-all-packages-installed-using-homebrew
            (loop for name in (execute-command host "brew" "list" :output :lines)
                  ;; this returns things very much like what we specify. I think that's probably fine
                  collect (adopt host (make-instance 'brew-package :name name))))))

(defmethod retrieve-installed-packages ((host pkgin-host))
  (append (call-next-method)
          (when (pkgin-installed-p host)
            (loop for line in (execute-command host "pkgin" "ls" :output :lines)
                  for bits = (cl-ppcre:split "\\s+" line)
                  collect (adopt host (make-instance 'pkgin-package
                                                     ;; We should get a version from this
                                                     :name (first bits)
                                                     :description (second bits)))))))


;; now, when we ask for the packages installed on a host this will retrieve them...
(defmethod installed-packages :before ((host unix-host))
  (unless (slot-boundp host 'installed-packages)
    (setf (installed-packages host)
          (retrieve-installed-packages host))))

;; (installed-packages (localhost))
;; (find-class 'darwin-localhost)

;; now we can come up with an update plan based on whether or not we want a package to be installed...

;; does it exist on the host?
;; !!! This should check the version etc
;; OTOH, it should return an older version - then our plan consists of updating it
(defmethod exists-p ((package software-package))
  (find (name package)
        (installed-packages (host package))
        :test #'equal :key #'name))

(defmethod exists-p ((package pkgin-package))
  (find (name package)
        (installed-packages (host package))
        :test (lambda (n a)
                (cl-ppcre:scan (format nil "~A-.*" n)
                               a))
        :key #'name))

;; this is basically just giving the create verb
(defmethod create-plan ((package software-package))
  `((install ,package)))

(defmethod destroy-plan ((package software-package))
  `((uninstall ,package)))

;; I wouldn't need this if I just used 'create' and 'destroy' as the verbs
;; I want to make it very explicit though
#+nil(defmethod update-plan ((package software-package) &optional without)
  (unless without
    ;; check version etc
    ))

;; For the moment the adopt protocol for changeing the class of the child classes isn't working. PROBABLY because of the ordering of super classes and hence the calling of initialize-instance methods
;; (update-plan (localhost (make-instance 'software-package :name "ledger")))
;; (update-plan (localhost (make-instance 'brew-package :name "rhubarb")))
;; (update-plan (localhost (without (make-instance 'brew-package :name "ledger"))))

;; execution of steps
(defmethod install ((package brew-package))
  (execute-command (host package)
                   "brew"
                   (list "install" (name package))))

;; this, of course, won't work.
;; (install (adopt (localhost) (make-instance 'brew-package :name "rhubarb")))

(defmethod uninstall ((package brew-package))
  (execute-command (host package)
                   "brew"
                   (list "uninstall" (name package))))


(defmethod install ((package pkgin-package))
  (execute-command (host package)
                   "pkgin" (list :y "in" (name package))))
