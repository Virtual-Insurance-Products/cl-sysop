
(in-package :cl-sysop)


;; Let's now start defining some internal zones - to run on the internal network
;; What to do if I want to run things on the admin network too?

(defclass internal-zone (joyent-zone)
  ()
  (:default-initargs
   :resolvers (list "10.10.0.2")
   :nics (list (make-instance 'smartos-nic :tag "stub0"))
   :image-uuid +base-64-feb-2020+))

;; no services unless explicitly given
(defmethod consul-services ((x internal-zone)) nil)

(defmethod initialize-instance :after ((x internal-zone) &rest opts)
  (declare (ignore opts))
  (unless (find 'consul-deployment
                (subcomponents x)
                :test (lambda (a b)
                        (typep b a)))
    (setf (slot-value x 'subcomponents)
          (append (subcomponents x)
                  (list (adopt x
                               (make-instance 'consul-deployment
                                              :name (alias x)
                                              :services (consul-services x))))))))

;; SO, the following is the set of steps for creating a new zone. Nice eh?
;; (update (sys (make-instance 'internal-zone :alias "asdf")))

;; Disable consul before destroying to remove from consul catalog
(defmethod destroy :before ((x internal-zone))
  (execute-command x "svcadm" (list "disable" "consul")))

(defclass isc-dhcpd-config ()
  ((start-ip :initarg :start-ip :reader start-ip)
   (end-ip :initarg :end-ip :reader end-ip)
   (opt::default-lease-time :initform 600 :initarg :default-lease-time)
   (opt::max-lease-time :initform 7200 :initarg :max-lease-time)
   ;; I'm just lazily naming these slots after dhcpd config options
   ;; I should probably pull the dhcpd config bits out of here
   (opt::option\ subnet-mask :initform "255.255.255.0" :initarg :subnet-mask :reader subnet-mask)
   (opt::option\ broadcast-address :initform "10.10.0.255" :initarg :broadcast-address :reader broadcast-address)
   (opt::option\ routers :initform "10.10.0.1" :initarg :routers)
   (opt::option\ domain-name-servers :initform "10.10.0.2" :initarg :domain-name-servers)
   ;; does this field need to be quoted? I'm not sure. I should read the manual
   (opt::option\ domain-name :initform "\"home.local\"")
   ))

;; (make-instance 'isc-dhcpd-config)

;; This is probably a naff way of getting this
(defmethod subnet ((x isc-dhcpd-config))
  (cl-ppcre:regex-replace "255$" (broadcast-address x) "0"))

(defmethod isc-dhcpd-config-file ((x isc-dhcpd-config) &key (full-path "/etc/dhcp/dhcpd.conf"))
  (make-instance 'fs-file
                 :full-path full-path
                 :content (with-output-to-string (stream)
                            (format stream "~%")
                            (loop for slot in (ccl:class-slots (class-of x))
                                  for name = (ccl:slot-definition-name slot)
                                  when (equal (package-name (symbol-package (ccl:slot-definition-name slot)))
                                              "OPT")
                                    do (format stream "~(~A~) ~A;~%" name (slot-value x name))
                                  )
                            (format stream "
subnet ~A netmask ~A {
    range ~A ~A;
}
" (subnet x) (subnet-mask x) (start-ip x) (end-ip x)))))

;; (isc-dhcpd-config-file (make-instance 'isc-dhcpd-config :start-ip "10.10.0.10" :end-ip "10.10.0.200"))

;; I should make the subnet configurable
(defclass internal-dhcp-and-dns (internal-zone isc-dhcpd-config)
  ()
  (:default-initargs
   :alias "internal-dhcp"
   :nics (list (make-instance 'smartos-nic
                              :tag "stub0"
                              :ip "10.10.0.2"
                              :netmask "255.255.255.0"
                              :allow-dhcp-spoofing t
                              :allow-ip-spoofing t
                              :primary t
                              :gateway "10.10.0.1"))))

;; !!! This should be able to infer the DHCP parameters (many of them anyway) from the primary interface of the VM
;; That would make it a lot more flexible and easier to deploy in other zones (eg for home)
;; In fact, it might justify replacing that function of the OpenBSD router. Or maybe not. 
(defmethod subcomponents ((x internal-dhcp-and-dns))
  (when (slot-boundp x 'parent)
    (mapcar
     (adopter x)
     (list
      (make-instance 'pkgin-package :name "isc-dhcpd")
      ;; This could be turned into a slot definition (this configuration)
      ;; I could also split out the various config bits into slots
      (isc-dhcpd-config-file x :full-path "/opt/local/etc/dhcp/dhcpd.conf")
      (make-instance 'smf-service :fmri "svc:/pkgsrc/isc-dhcpd:default")
      (make-instance 'pkgin-package :name "unbound")
      (make-instance 'fs-file
                     :full-path "/opt/local/etc/unbound/unbound.conf"
                     :content "
server:
	interface: 10.10.0.2
	interface: 127.0.0.1
	access-control: 10.10.0.0/24 allow
	do-not-query-localhost: no
	hide-identity: yes
	hide-version: yes
        domain-insecure: \"consul\"

forward-zone:
        name: \".\"
        forward-addr: 1.1.1.1  # IP of the upstream resolver

# Add consul as a stub-zone
stub-zone:
        name: \"consul\"
        stub-addr: 10.10.0.1@8600

")
      (make-instance 'smf-service :fmri "svc:/pkgsrc/unbound:default")
      (make-instance 'consul-deployment
                     :name "internal-dhcp"
                     :services (list (make-instance 'consul-service
                                                    :name "dns"
                                                    :port 53
                                                    :check (tcp-check "10.10.0.2:53"))))))))


