
(in-package :cl-sysop)

;; now, at some point I need a general way of running commands against a system shell
;; I need to be able to read from them

(defclass host-system (system) ())

(defclass unix-host (host-system named)
  ;; this just reifies the subcomponents in a simple slot
  ((subcomponents :initarg :subcomponents :initform nil :reader subcomponents)
   ;; this is used to cache the list of installed packages
   ;; it will be cleared before the plan phase
   (installed-packages :accessor installed-packages)
   (existing-services :accessor existing-services)))

(defmethod update-plan :before ((host unix-host) &optional without)
  (declare (ignore without))
  (slot-makunbound host 'installed-packages)
  (slot-makunbound host 'existing-services))


;; shall we just assume this will always be running on a unix host?
(defclass localhost (unix-host)
  ())

;; Let's assume that unix hosts we talk about exist
;; this won't be the case for solaris zones or DO droplets or other creatable things
;; but since we can't create all kinds of host let's assume this by default
(defmethod exists-p ((host unix-host)) t)

;; this is a host we can SSH into to get a command shell
(defclass sshd-host (unix-host)
  ((user :initarg :user :reader user)
   (access-from :initarg :access-from :initform localhost :reader access-from)
   ;; allow specification of which key to use for which host.
   (key :initarg :key :reader key)))



;; has SMF, pkgin
(defclass solaris-host (pkgin-host unix-host)
  ())

;; this could also have pkgin. What to do about that?
;; maybe the pkgin-host should provide a flag indicating whether it's installed
;; Good Idea
(defclass darwin-host (brew-host pkgin-host unix-host)
  ()
  ;; this isn't working. Must try and fix
  (:default-initargs :pkgin-installed-p nil))

(defclass solaris-global-zone (solaris-host)
  ())

;; and this is a smartos host
(defclass smartos-host (sshd-host solaris-global-zone)
  ;; these things without initargs or writers are read only properties for interrogating running systems
  ;; to specify that you WANT a vm to exist on a host just put it as one of the subcomponents
  ((vms :reader vms)))

;; Clear the cached vms before starting to plan
(defmethod update-plan :before ((host smartos-host) &optional without)
  (declare (ignore without))
  (slot-makunbound host 'vms))

;; For stuff we can convert into a json description
(defclass vmadm-json-object () ())

(deftype object-list () 'list)

;; general properties of a zone - there are lots more
;; 
(defclass smartos-zone (component vmadm-json-object)
  ;; uuid will probably be pulled into a superclass
  ((vmadm::uuid :reader uuid)
   vmadm::type
   vmadm::ram
   ;; desired state
   (vmadm::state :initform "running" :initarg :state :reader state)
   (vmadm::alias :reader alias :initarg :alias)
   vmadm::customer_metadata.source_uuid
   (vmadm::image_uuid :initarg :image-uuid :reader image-uuid)

   (vmadm::max_physical_memory :accessor max-physical-memory :initarg :max-physical-memory :initform 256)
   (vmadm::quota :initform 0 :initarg :quote :reader quota)
   (vmadm::resolvers :initform (list "8.8.8.8" "8.8.4.4") :initarg :resolvers :reader resolvers)

   (vmadm::nics :initarg :nics :reader nics :type object-list)

   (current-specification :reader current-specification)
   
   ))

(defmethod update-plan :before ((host smartos-zone) &optional without)
  (declare (ignore without))
  (slot-makunbound host 'current-specification))

(defmethod name ((x smartos-zone))
  (if (slot-boundp x 'name)
      (call-next-method)
      (alias x)))

(defmethod print-object ((instance smartos-zone) stream)
  (if (slot-boundp instance 'vmadm::alias)
      (print-unreadable-object (instance stream)
        (let* ((class (class-of instance))
               (class-name (class-name class)))
          (format stream "~A ~A" class-name (alias instance))))
      (call-next-method)))

;; this is a component as well as a system (which will be very common)
(defclass joyent-zone (smartos-zone solaris-host component) ())

(defmethod brand ((j joyent-zone)) "joyent")

(defclass lx-zone (smartos-zone unix-host component)
  ((type :initform "LX")))



(defclass darwin-localhost (darwin-host localhost) ())
(defclass solaris-localhost (solaris-host localhost) ())
(defclass linux-localhost (darwin-host localhost) ())

;; try and get a more specific instance
(defmethod initialize-instance :after ((x localhost) &rest initargs)
  (unless (slot-boundp x 'name)
    (setf (name x)
          (execute-command x
                           "hostname" nil :output :first-line)))
  (when (eq 'localhost (type-of x))
    ;; etc
    #+darwin (change-class x 'darwin-localhost)
    #+solaris (change-class x 'solaris-localhost)
    #+linux (change-class x 'linux-localhost)

    ;; call the initialize method again in case it has more work to do now it knows what /type/ of host it is
    (apply #'initialize-instance (cons x initargs))))


;; (make-instance 'localhost)

;; I know this should have earmuffs, but they get boring to type
;; don't use this instance to create systems on localhost though - we don't want to keep replacing the subcomponents
;; is this a nice way to construct things? Probably
(defun localhost (&rest components)
  (make-instance 'localhost :subcomponents components
                 ;; I'm going to put this here to stop spurious hostname commands
                 ;; use make-instance if you want to find out the name
                 :name "localhost"))


;; When we consider local-host we should further identify it by running uname if needed


(defmethod host ((x component))
  (if (typep (parent x) 'host-system)
      (parent x)
      (host (parent x))))



(defmethod make-shell-command-for-host ((host localhost) command args)
  (make-shell-command command args))

;; command execution
;; we could have done this without the shell then we wouldn't need to do all the escaping
;; the problem is, if we're going to use SSH to get to other things we have to deal with the shell
;; so we kind of might as well.
(defmethod execute-command ((host unix-host) command args &key input
                                                            (output :string) ; :string :lines or a stream
                                                            (line-reader nil)
                                                            (echo-command t)
                                                            (error (make-string-output-stream)))
  (execute-with-shell (make-shell-command-for-host host command args)
                      :input (if (stringp input)
                                 (make-string-input-stream input)
                                 input)
                      :output output
                      :line-reader line-reader
                      :echo-command echo-command
                      :error error))


;; (execute-command (localhost) "ls" '("/bin") :output :lines)
;; (execute-command (localhost) "ps" '("aux") :output *standard-output*)
;; (execute-command (localhost) "cat" "/etc/fstab")
;; (execute-command (localhost) "cat" "/etc/hosts")
;; (execute-command (localhost) "cat" "/etc/not-here")
;; (execute-command (localhost) "blah" "/etc/more-hosts")

;; redirection to a file
;; (execute-command (localhost) "cat" '((> "/Users/david/test.txt")) :input "hi there")
;; (execute-command (localhost) "cat" "/Users/david/test.txt")
;; (I did have things like pipelines before in Eve - I'll get to that)

;; I should make a better error handler. It would also be nice
;; until I can find a way to capture standard error


;; now that we can execute things on localhost I can use ssh to execute things on remote hosts...

(defmethod make-shell-command-for-host ((host sshd-host) command args)
  ;; recurse...
  (make-shell-command-for-host (access-from host)
                               "ssh"
                               `(,(if (slot-boundp host 'user)
                                      (concatenate 'string
                                                   (user host) "@" (name host))
                                      (name host))
                                 ,@ (when (slot-boundp host 'key)
                                      (list :i (key host)))
                                 ,(make-shell-command command args))))


;; (execute-command (make-instance 'sshd-host :name "liganc") "uname" :a :output :first-line)
;; (execute-command (make-instance 'sshd-host :name "liganc") "cat" '((> "/tmp/greetings.txt")) :input "Hi there")
;; (execute-command (make-instance 'sshd-host :name "liganc") "cat" "/tmp/greetings.txt")
;; (execute-command (make-instance 'sshd-host :name "liganc") "rm" "/tmp/greetings.txt")

;; access 1 ssh host from another -doesn't matter.  This could be chained arbitrarily
;; (execute-command (make-instance 'sshd-host :name "149.202.74.229" :user "root" :access-from (make-instance 'sshd-host :name "ovh2")) "uname" :a)

;; there might be other things we need to do here - rsync from one system to another. SSH allows all sorts of shenanigans, which could be configured here. 

;; (execute-command (localhost) "echo" '((> "/Users/david/foo.txt")) :input "new file")
;; (execute-command (localhost) "rm" "/Users/david/foo.txt")
;; (execute-command (localhost) "test" '("-f" "/Users/david/foo.txt")) ; error 1 if not existing

;; we can detect that error like this, so this is a reliable way to test for files. I think.
;; although what if there's a different error? 
;; (handler-case (execute-command (localhost) "test" '("-f" "/Users/david/foo.txt")) (shell-error (c) (error-code c)))


(defmethod make-shell-command-for-host ((host smartos-zone) command args)
  (make-shell-command-for-host (parent host)
                               "zlogin"
                               (list (uuid host)
                                     (make-shell-command command args))))
