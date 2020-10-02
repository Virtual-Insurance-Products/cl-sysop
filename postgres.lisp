
(in-package :cl-sysop)
;; Let's create databases...

;; So far this works in SmartOS zones only and needs several more config options
;; in particular to control the postgres config file in terms of listening interfaces etc

(defclass postgres-server (system component)
  ((version :initform 12 :reader version)
   ;; Should this really be a file? Do we want the file to be put in place? 
   (initialise-from :initarg :initialise-from
                    :type (or string fs-file)
                    :reader initialise-from)
   (restore-from :initarg :restore-from
                 :type (or string fs-file)
                 :reader restore-from)
   ))

(defmethod subcomponents ((x postgres-server))
  (when (slot-boundp x 'parent)
    (mapcar
     (lambda (sub)
       (adopt x sub))
     (list (make-instance 'pkgin-package
                          :name (format nil "postgresql~A-server"
                                        (version x)))
           ;; the create plan for smf-service should probably just be enable
           (make-instance 'fs-file
                          ;; this path might have to be conditionalised on the host type
                          :full-path "/var/pgsql/data/pg_hba.conf"
                          :content "
local all postgres peer


local   all             all                                     password
# IPv4 local connections:
host    all             all             127.0.0.1/32            password
# IPv6 local connections:
host    all             all             ::1/128                 password
# Allow replication connections from localhost, by a user with the
# replication privilege.
local   replication     all                                     password
host    replication     all             127.0.0.1/32            password
host    replication     all             ::1/128                 password

")
           
           ;; put in some configuration files
           ;; I'm somehow going to have to abstract this out. 
           (make-instance 'smf-service :name "postgresql")
           ))))

(defmethod restart-action ((x postgres-server))
  `(restart ,(find 'smf-service
                   (subcomponents x)
                   :test #'(lambda (a b)
                             (typep b a)))))

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
