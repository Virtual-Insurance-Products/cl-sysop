
(in-package :cl-sysop)
;; Let's create databases...

;; So far this works in SmartOS zones only and needs several more config options
;; in particular to control the postgres config file in terms of listening interfaces etc


(defun memory-amount-p (str)
  (cl-ppcre:scan "^\\d+(kB|MB|GB|TB)$" str))

(defclass postgres-server (system component postgres-config)
  ((version :initform 12 :reader version)
   ;; Should this really be a file? Do we want the file to be put in place? 
   (initialise-from :initarg :initialise-from
                    :type (or string fs-file)
                    :reader initialise-from)
   (restore-from :initarg :restore-from
                 :type (or string fs-file)
                 :reader restore-from)

   (pg::port :initform 5432 :initarg :port :type (integer 1 65535)
             :documentation "Sets the TCP port the server listens on."
             ) ; could probably be further constrained
   (pg::max_connections :initform 100 :initarg :max-connections :type integer
                        :documentation "Sets the maximum number of concurrent connections."                        
                        )
   (pg::listen_addresses :initform "localhost" :initarg :listen-addresses
                         :documentation "Sets the host name or IP address(es) to listen to.")
   (pg::shared_buffers :initform "128MB" :initarg :shared-buffers :type (and string
                                                                             (satisfies memory-amount-p))
                       :documentation "Sets the number of shared memory buffers used by the server.")
   
   ;; I should probably find a better way to model these, but this will suffice to begin with
   (default-host-based-access :initform (list "local all postgres peer"
                                              "local   all             all                                     password"
                                              "host    all             all             127.0.0.1/32            password"
                                              "host    all             all             ::1/128                 password"
                                              "local   replication     all                                     password"
                                              "host    replication     all             127.0.0.1/32            password"
                                              "host    replication     all             ::1/128                 password")
                              :initarg :default-host-based-access
                              :reader default-host-based-access)
   ;; more HBA so we don't have to restate all the default ones
   (host-based-access :initform nil :initarg :host-based-access :reader host-based-access)))


(defmethod config-file ((x postgres-server))
  (make-instance 'fs-file
                 :name "postgresql.conf"
                 ;; genereate config file from certain properties
                 :content
                 (with-output-to-string (stream)
                   (loop for slot in (ccl:class-slots (class-of x))
                         for name = (ccl:slot-definition-name slot)
                         when (and (slot-boundp x name)
                                   (equal (package-name (symbol-package name)) "PG"))
                           do (let ((value (slot-value x name)))
                                (format stream
                                        "~(~A~) = ~A~%"
                                        (symbol-name name)
                                        (cond ((stringp value)
                                               ;; Do I need to do SQL escapes here? Does this use SQL format?
                                               (format nil "'~A'" value))
                                              ((eq t value) "on")
                                              ((null value) "off")
                                              ((eq 'boolean (ccl:slot-definition-type slot))
                                               (if value "yes" "no"))
                                              (t value))))))))


;; (config-file (make-instance 'postgres-server))

(defmethod subcomponents ((x postgres-server))
  (when (slot-boundp x 'parent)
    (mapcar
     (lambda (sub)
       (adopt x sub))
     (list (make-instance 'pkgin-package
                          :name (format nil "postgresql~A-server"
                                        (version x)))
           ;; the create plan for smf-service should probably just be enable
           ;; The following should be factored out
           (make-instance 'fs-directory
                          :full-path "/var/pgsql/data"
                          :content
                          (list 
                           (make-instance 'fs-file
                                          ;; this path might have to be conditionalised on the host type
                                          :name "pg_hba.conf"
                                          :content (vip-utils:string-list
                                                    (append (default-host-based-access x)
                                                            (host-based-access x))
                                                    (format nil "~%")))

                           (config-file x)))
           
           ;; put in some configuration files
           ;; I'm somehow going to have to abstract this out. 
           (make-instance 'smf-service :name "postgresql")
           ))))


(defmethod restart-action ((x postgres-server))
  `(restart ,(find 'smf-service
                   (subcomponents x)
                   :test #'(lambda (a b)
                             (typep b a)))))

;; This can be made more nuanced - some config changes reuireq restarting the database but some don't
(defmethod update-plan ((x postgres-server) &optional without)
  (if without
      (call-next-method)
      (let ((sub (call-next-method)))
        (append sub
                (when (slot-boundp x 'restore-from)
                  `((restore-database ,x ,(restore-from x))))
                (when sub
                  (list (restart-action x)))))))

(defmethod create-plan ((x postgres-server))
  (append (call-next-method)
          ;; maybe this shouldn't be in the update action
          (when (slot-boundp x 'initialise-from)
            `((restore-database ,x ,(initialise-from x))))))

(defmethod psql-command ((x postgres-server) &key input file)
  (execute-command (host x)
                   "su"
                   (list "postgres" :c
                         (if file
                             (format nil "psql -f ~S" file)
                             "psql"))
                   :input input))

;; This is a thin wrapper to make the plan clear
;; Is that really needed? Might make sense
(defmethod restore-database ((x postgres-server) (file string))
  (psql-command x :file file))

(defmethod restore-database ((x postgres-server) (file fs-file))
  ;; put the file in a temporary place if it's on a different host
  (error "Not yet implemented")
  )

;; It would be better if we could use the postgres library to connect into this
;; can I do that easily enough? 
;; I don't want to parse string output
;; I need to know whether the 
#+nil(psql-command (adopt
                    (adopt (ovh2)
                           (make-instance 'joyent-zone
                                          :alias "dtw-test-database"))
                    (make-instance 'postgres-server))
                   "\\l")
