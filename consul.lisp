
(in-package :cl-sysop)


;; Things for deploying consul.
;; We need to handle certificates like we do with openssl, but with consul

;; ACTUALLY I probably don't need to use these things to generate the CA.
;; I think I can just use my openssl code. I know these are provided as a convenience.
;; I might try them though.
;; I wonder if I can verify the certificates with openssl. I expect so.

;; I can probably generalise these things - we're going to need the same sort of thing for SSH identities too
;; and many other things
;; Once I have all these things worked out I'll be able to do a lot of good, secure things with them
;; I might even start using shyshy properly
(defclass consul-identity ()
  ((certificate :initarg :certificate :reader certificate)
   (key :initarg :key :reader key)))

(defclass consul-ca (consul-identity)
  ())

;; Although I could generalise this to other consul identities I don't think it will ever be needed to do so
(defmethod print-object ((ca consul-ca) (s stream))
  (format s
          "~S"
          `(make-instance ',(class-name (class-of ca))
                          :certificate ,(certificate ca)
                          :key ,(key ca))))

(defmethod files ((ca consul-ca))
  (mapcar (lambda (f)
            (adopt (localhost) f))
          (list (make-instance 'fs-file :full-path "consul-agent-ca.pem"
                                        :content (certificate ca))
                (make-instance 'fs-file :full-path "consul-agent-ca-key.pem"
                                        :content (key ca)))))

(defun make-consul-ca ()
  (in-temporary-directory nil
    (execute-command (localhost)
                     "consul"
                     (list "tls" "ca" "create"))
    ;; (execute-command (localhost) "ls" "-l")
    ;; !!! Couldn't I use the files bit? But I need them first. 
    (make-instance 'consul-ca
                   :certificate (existing-content (adopt (localhost) (make-instance 'fs-file :full-path "consul-agent-ca.pem")))
                   :key (existing-content (adopt (localhost) (make-instance 'fs-file :full-path "consul-agent-ca-key.pem"))))
    ))


;; !!! Finish this
;; it needs server vs client certificates
;; I don't think I need to generate client ones, but maybe I could
(defun make-consul-identity (ca &key (dc-name "dc1") (server t))
  (in-temporary-directory nil
    (mapcar #'create (files ca))
    (execute-command (localhost)
                     "consul"
                     (list "tls" "cert" "create" (if server "-server" "-client")
                           "-dc" dc-name))
    (make-instance 'consul-identity
                   :certificate (existing-content (adopt (localhost)
                                                         (make-instance 'fs-file
                                                                        :full-path (format nil "~A-~A-consul-0.pem"
                                                                                           dc-name
                                                                                           (if server
                                                                                               "server" "client")))))
                   :key (existing-content (adopt (localhost)
                                                 (make-instance 'fs-file
                                                                :full-path (format nil "~A-~A-consul-0-key.pem"
                                                                                   dc-name
                                                                                   (if server
                                                                                       "server" "client"))))))))


;; now you can just evaluate the following printing the result into the code...
;; (defparameter *consul-ca* (make-consul-ca))

;; (make-consul-identity *consul-ca* :dc-name "ovh1")

;; It would be good if I could implement the same checking things that I did for openssl so that I don't need to generate new certificates for servers if I've made ones which are ok. Can I?
;; IF not, do I have to use consul or can openssl check these things for me?

;; something which lives (notionally) in a consul dc
(defclass consul-dc-component ()
  ((json-property::datacenter :initarg :datacenter :initform "dc1" :reader datacenter)))

(defclass consul-certificate-pair (rsa-certificate-pair consul-dc-component)
  ((consul-ca :initarg :consul-ca :reader consul-ca)
   (server-p :initarg :server-p :initform nil :reader server-p)))

(defmethod initialize-instance :after ((pair consul-certificate-pair) &rest args)
  (declare (ignore args))
  (unless (slot-boundp pair 'name)
    (setf (name pair)
          (if (server-p pair)
              "server" "client")))
  ;; copy the bits from the consul CA so I can verify stuff
  (setf (certificate-authority pair) (certificate (consul-ca pair))
        (certificate-authority-key pair) (key (consul-ca pair))

        (common-name pair) (concatenate 'string
                                        (name pair) "."
                                        (datacenter pair)
                                        ".consul")))



;; This is probably a more logical way of doing things than the openssl way - have an object representing the identity
;; I could just rely on openssl to generate this for me instead. Especially now that I've figured out what the CN
;; is supposed to be
;; I probably don't need the consul CA stuff at all.
;; I might try removing this create method
;; It's fine to hvae the consul-certificate-pair subclass as it just defines the relevant consul information
;; which is then used to populate certificate details
(defmethod create ((pair consul-certificate-pair))
  (let ((id (make-consul-identity (consul-ca pair)
                                  :dc-name (datacenter pair)
                                  :server (server-p pair))))
    (setf (slot-value (part pair "cert") 'content) (certificate id)
          (slot-value (part pair "key") 'content) (key id))))



(defun make-gossip-key ()
  (execute-command (localhost)
                   "consul" "keygen"
                   :output :first-line))
;; (make-gossip-key)

(defclass consul-deployment (named system component json-object consul-dc-component)
  ((binary :initform (make-instance 'hashicorp-binary :name "consul")
           :reader binary)
   (config-dir :accessor config-dir :initform "/opt/consul/etc")
   (services :initarg :services :reader services :initform nil)
   (certificate-authority :initarg :certificate-authority :reader certificate-authority)

   (json-property::ca_file :initarg :ca-file :initform "/opt/consul/ca.pem")
   (json-property::verify_incoming :initarg :verify-incoming :initform nil :reader verify-incoming)
   (json-property::verify_outgoing :initarg :verify-outgoing :initform t :reader verify-outgoing)

   (json-property::encrypt :initarg :gossip-key :initform (error "Specify a gossip key generated with (make-gossip-key)"))

   (json-property::start_join :type list :initarg :start-join)
   ;; I don't know if I need this as well
   ;; (json-property::retry_join :type list :initarg :retry-join)
   (json-property::verify_server_hostname :initarg :verify-server-hostname :initform t)
   (json-property::data_dir :initarg :data-dir :initform "/opt/consul/data")
   (json-property::log_level :initarg :log-level :initform "INFO")

   (json-property::bind_addr :initarg :bind-addr :type string)
   
   (json-property::auto_encrypt :initform '((:tls . t)))))



;; I might need an after initialize instance to create some of the other components based on defined properties


(defmethod consul-etc-directory ((s consul-deployment))
  (make-instance 'fs-directory
                 :name "etc"
                 :exclude-others t
                 :content
                 ;; !!! Add the service descriptions which are registered directly 
                 (append
                  (list (make-instance 'fs-file :name "_consul.json"
                                                :content
                                                (json:encode-json-to-string
                                                 (json-spec s))))
                  (mapcar (lambda (service)
                            (make-instance 'fs-file
                                           :name (concatenate 'string
                                                              (name service)
                                                              ".json")
                                           :content (json:encode-json-to-string
                                                     (json-spec service))))
                          (services s)))))

(defmethod subcomponents ((s consul-deployment))
  (cons (binary s)
        (when (slot-boundp s 'parent)
          (list (adopt s
                       (make-instance 'fs-directory
                                      :full-path "/opt/consul"
                                      :exclude-others t
                                      :content (list
                                                (make-instance 'fs-directory :name "data" :content nil)
                                                (make-instance 'fs-file
                                                               :name "ca.pem"
                                                               :content (certificate (certificate-authority s)))
                                                (consul-etc-directory s))))
                  
                (make-instance 'custom-smf-service
                               :parent s
                               :name "consul"
                               :instances '("server")
                               :start-command
                               (format nil "/opt/local/bin/consul agent -config-dir=~S -ui"
                                       (config-dir s)))))))

;; ALSO, if I don't have unzip installed then I've got to add that to the update plan. That's ok
(defmethod update-plan ((c consul-deployment) &optional without)
  (if without
      (call-next-method)
      (let ((plan (reduce #'append
                          (mapcar #'update-plan
                                  (subcomponents c)))))
        (when plan
          (append plan
                  (let ((service (find 'smf-service
                                       (subcomponents c)
                                       :test (lambda (a b) (typep b a)))))
                    `((restart ,c)
                      (check ,service))))))))

;; restarting consul (and reloading config) is easy
(defmethod restart ((c consul-deployment))
  (execute-command (host c)
                   "consul" "reload"))

;; this is a server node
(defclass consul-server (consul-deployment)
  ((bootstrap-expect :initarg :bootstrap-expect :initform 1 :reader bootstrap-expect)
   (name :initform "dc-server")

   (json-property::cert_file :initarg :cert-file :initform "/opt/consul/server.cert")
   (json-property::key_file :initarg :cert-file :initform "/opt/consul/server.key")
   (json-property::server :initform t)
   (json-property::advertise_addr :initarg :advertise-addr :type string)
   (json-property::primary_datacenter :initarg :primary-datacenter :type string
                                      :initform "dc1")
   (json-property::auto_encrypt :initform '((:allow_tls . t)))
   )
  (:default-initargs
   :verify-incoming t
   :verify-outgoing t
   ))

(defmethod subcomponents ((s consul-server))
  (cons (binary s)
        (when (slot-boundp s 'parent)
          (list (adopt s
                       (make-instance 'fs-directory
                                      :full-path "/opt/consul"
                                      :exclude-others t
                                      :content (list
                                                (make-instance 'fs-directory :name "data" :content nil)
                                                (make-instance 'fs-file :name "ca.pem"
                                                                        :content (certificate
                                                                                  (certificate-authority s)))
                                                ;; this is generated by consul, but could be generated by openssl instead
                                                ;; openssl verifies it for us
                                                (make-instance 'consul-certificate-pair
                                                               :server-p t
                                                               :datacenter (datacenter s)
                                                               :consul-ca (certificate-authority s))
                                                (consul-etc-directory s))))
                
                (make-instance 'custom-smf-service
                               :parent s
                               :name "consul"
                               :instances '("server")
                               :start-command
                               (format nil "/opt/local/bin/consul agent -bootstrap-expect=~A -config-dir=~S -ui"
                                       (bootstrap-expect s)
                                       (config-dir s)))))))

(defmethod json-spec ((c consul-server))
  (append `((:auto_encrypt . ((:allow_tls . t)))
            (:node_name . ,(name c)))
          (call-next-method)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Consul service descriptions

(defclass consul-service-check (json-object)
  ((json-property::interval :initarg :interval :initform "10s")
   (json-property::timeout :initarg :timeout :initform "2s")
   (json-property::id :initarg :id)
   ;; to give is an initial state before performing the first check
   (json-property::status :initarg :status :initarg :initial-state)))

(defclass consul-http-check (consul-service-check)
  ((json-property::http :type :string :initarg :http)
   (method :type (member :get :post) :initarg :method :reader method)
   ))

(defclass consul-tcp-check (consul-service-check)
  ((json-property::tcp :initarg :tcp :type string)))

(defun http-check (url &key (method :get)
                         (timeout "2s")
                         (interval "10s"))
  (make-instance 'consul-http-check :http url
                                    :method method
                                    :interval interval
                                    :timeout timeout))

(defun tcp-check (host-and-port &key (timeout "1s") (interval "10s"))
  (make-instance 'consul-tcp-check :tcp host-and-port :timeout timeout :interval interval))

(defmethod json-spec ((c consul-http-check))
  (append (call-next-method)
          `((:method . ,(string-upcase (method c))))))

;; Script checks for checking all kinds of things


;; now I can start to describe services to consul
;; I should also be able to put in the Traefik information in these things...
(defclass consul-service (json-named component)
  ((json-property::address :initarg :address :reader address :type string)
   (json-property::port :initarg :port :reader port :type (integer 1 65535))
   (json-property::tags :initarg :tags :type list :reader tags)
   (json-property::check :initarg :check :type consul-service-check :reader check)
   ))

(defmethod json-spec ((s consul-service))
  `((:service . ,(call-next-method))))





;; ALERTING

;; https://github.com/AcalephStorage/consul-alerts
