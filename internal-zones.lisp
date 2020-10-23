
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


;; I should make the subnet configurable
(defclass internal-dhcp-and-dns (internal-zone)
  ((start-ip :initarg :start-ip :reader start-ip)
   (end-ip :initarg :end-ip :reader end-ip))
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

(defmethod subcomponents ((x internal-dhcp-and-dns))
  (when (slot-boundp x 'parent)
    (mapcar
     (adopter x)
     (list
      (make-instance 'pkgin-package :name "isc-dhcpd")
      (make-instance 'fs-file
                     :full-path "/opt/local/etc/dhcp/dhcpd.conf"
                     :content (format nil "
default-lease-time 600;
max-lease-time 7200;

option subnet-mask 255.255.255.0;
option broadcast-address 10.10.0.255;
option routers 10.10.0.1;
option domain-name-servers 10.10.0.2;
option domain-name \"home.local\";

subnet 10.10.0.0 netmask 255.255.255.0 {
    range ~A ~A;
}
" (start-ip x)
  (end-ip x)))
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
