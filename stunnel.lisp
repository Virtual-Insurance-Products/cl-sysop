
(in-package :cl-sysop)

;; This gives a list of SNI services that we want to route in to;

;; I think that's all I need for the stunnel configuration
(defun host-sni-exposed-services (host to)
  (let ((consul (adopt host (make-instance 'consul-deployment))))
    (loop for service in (catalog-services consul)
          for sni = (loop for (nil . value) in (tags-alist service)
                          when (cl-ppcre:scan "HostSNI" value)
                            return (cl-ppcre:regex-replace ".*\\`(.*)\\`.*" value "\\1"))
          when (and (find (format nil "proxyto-~A" (name to))
                          (tags service) :test #'equal)
                    sni)
            collect (let ((port (port (first (catalog-service consul (name service))))))
                      (list (name service) sni port)))))

;; (host-sni-exposed-services (ovh1) (ovh2))
;; (host-sni-exposed-services (ovh2) (ovh1))

;; (catalog-services (adopt (ovh1) (consul-deployment nil)))
;; (catalog-service (adopt (ovh1) (consul-deployment nil)) "abel-database")

;; Define an stunnel zone which will proxy things

(defclass stunnel-zone (internal-zone)
  ((proxy-from :initarg :proxy-from :reader proxy-from)
   (traefik-ca-cert :initarg :traefik-ca-cert :reader traefik-ca-cert)
   (traefik-ca-key :initarg :traefik-ca-key :reader traefik-ca-key))
  (:default-initargs :alias "stunnel-proxy"))

;; what would be good would be to synthesis the output of the following:-
;; (vms (ovh1))
;; with a catalog of defined VMs.

(defmethod subcomponents ((x stunnel-zone))
  (when (slot-boundp x 'parent)
    (let ((services-to-proxy (host-sni-exposed-services (proxy-from x) (parent x)))
          (port-map (make-hash-table))
          ;; this is the 'output' map used for the consul service registrations
          (services))
      (mapcar (adopter x)
              (append (list (make-instance 'pkgin-package :name "stunnel")
                            (make-instance 'fs-directory
                                           :full-path "/etc/stunnel"
                                           :exclude-others t
                                           :content (list
                                                     (make-instance
                                                      'fs-file :name "stunnel.conf"
                                                      :content
                                                      (with-output-to-string (stream)
                                                        (loop for (name sni port) in services-to-proxy
                                                              do
                                                                 (loop while (gethash port port-map)
                                                                       do (incf port))
                                                                 (setf (gethash port port-map) t)
                                                                   
                                                                 ;; note the port used for each service
                                                                 ;; that way we can advertise it
                                                                 (push (cons name port) services)
                                                                 (format stream "
[~A]
client = yes
sni = ~A
connect = ~A:443
accept =  0.0.0.0:~A
verify = 2
cafile = /etc/stunnel/ca.cert
key = /etc/stunnel/client.key
cert = /etc/stunnel/client.cert

"
                                                                         name sni sni port))))
                                                     (make-instance 'fs-file
                                                                    :name "ca.cert"
                                                                    :content (traefik-ca-cert x))
                                                     
                                                     (make-instance 'rsa-certificate-pair
                                                                    :ca (traefik-ca-cert x)
                                                                    :ca-key (traefik-ca-key x)
                                                                    :name "client"
                                                                    :common-name "stunnel.local")
                                                     ))
                            (make-instance 'consul-deployment
                                           :name "stunnel"
                                           :services
                                           (loop for (name . port) in services
                                                 ;; I don't really need to proxy slime
                                                 collect (unless (cl-ppcre:scan "slime" name)
                                                           (make-instance 'consul-service
                                                                          :tags '("proxied-service")
                                                                          :port port
                                                                          :name name
                                                                          :check (tcp-check (format nil "localhost:~A" port))))))
                            ;; now to describe the service...
                            (make-instance 'custom-smf-service
                                           ;; :fmri "svc:/stunnel:default"
                                           :name "stunnel"
                                           :start-command "/opt/local/bin/stunnel /etc/stunnel/stunnel.conf")))))))



