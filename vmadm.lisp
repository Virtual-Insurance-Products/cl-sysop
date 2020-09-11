
(in-package :cl-sysop)

;; I'm just using this for json generation to avoid name conflicts...
(defpackage :vmadm)

;; obviously there's a lot more information we could get here using
(defmethod vms :before ((host smartos-host))
  (unless (slot-boundp host 'vms)
    (setf (slot-value host 'vms)
          (let ((fields '(uuid type ram state alias
                          customer_metadata.source_uuid image_uuid
                          )))
            (loop for line in (execute-command host
                                             "vmadm"
                                             (list "list" "-H"
                                                   ;; I could usefully get more fields than this
                                                   "-o" (format nil "~(~{~a~^,~}~)" fields))
                                             :output :lines)
                for values = (cl-ppcre:split "\\s+" line)
                  for type = (second values)
                collect (let ((vm (adopt host
                                         (make-instance (cond ((equal type "OS")
                                                               'joyent-zone)
                                                              ((equal type "LX")
                                                               'lx-zone)
                                                              (t 'smartos-zone))))))
                          (loop for field in fields
                                for value in values
                                do (setf (slot-value vm field) value))
                          vm))))))

;; somehow this all needs to be parsed and put into the slots of the instance vm
(defmethod vmadm-get ((x smartos-zone))
  (execute-command (parent x)
                   "vmadm" (list "get" (uuid x))))

;; What if we specify 2 identifying slots to narrow it down? Don't know
(defmethod find-zones ((host smartos-zone))
  (loop for identifying-slot in '(alias)
        when (slot-boundp host identifying-slot)
          return (remove (slot-value host identifying-slot)
                         (vms (parent host))
                         :test-not 'equal
                         :key (lambda (x)
                                (slot-value x identifying-slot)))))

(defmethod exists-p ((host smartos-zone))
  (let ((found (find-zones host)))
    (when (cdr found)
      (error "Too many zones match ~A" host))
    (first found)))

(defmethod uuid :before ((host smartos-zone))
  (unless (slot-boundp host 'uuid)
    ;; we need a way to find the zone described
    ;; we will look for certain slots in order...
    (when (exists-p host)
      (setf (slot-value host 'uuid)
            (or (uuid (exists-p host))
                (error "Zone not found ~A" host))))))

(defparameter +base-64-feb-2020+ "ad6f47f2-c691-11ea-a6a5-cf0776f07bb7")

;; I should really just name these slots according to the json name
(defclass smartos-nic ()
  ((vmadm::nic_tag :initform "admin" :initarg :tag :reader tag)
   (vmadm::netmask :initarg :netmask)
   (vmadm::gateway :initarg :gateway)
   (vmadm::ip :initarg :ip :reader ip-address :type (or string (eql :dhcp))
              :initform :dhcp)
   (vmadm::primary :initarg :primary)))

(defun primary-nic (tag &rest options)
  (apply #'make-instance `(smartos-nic :tag ,tag ,@options)))

;; !!! Pull this into a JSON serialisable or something
;; !!! Also, ip is actually deprecated
(defmethod json-spec ((x smartos-nic))
  (loop for slot in (ccl:class-slots (class-of x))
        for name = (ccl:slot-definition-name slot)
        when (and (equal (package-name (symbol-package name)) "VMADM")
                  (slot-boundp x name))
          collect (cons name (slot-value x name))))


;; (json:encode-json-to-string (json-spec (make-instance 'smartos-nic :primary t)))
;; (json:encode-json-to-string (json-spec (primary-nic "admin")))
#+nil(json:encode-json-to-string (json-spec (make-instance 'smartos-nic :primary t
                                                                   :ip "10.10.0.123"
                                                      :netmask "255.255.255.0"
                                                                   :gateway "10.10.0.1"
                                                      :tag "stub0")))

;; !!! There's more to add in here and I think I should rename some slots to match things
;; The only thing is: what if we want to use readers?
;; Well, then we should get the accessor or reader for the slot (using MOP) and call that
;; I'll get to that...
(defmethod json-spec ((vm smartos-zone))
  `((brand . ,(brand vm))
    (hostname . ,(name vm))
    (image_uuid . ,(image-uuid vm))
    (alias . ,(alias vm))
    (max_physical_memory . ,(max-physical-memory vm))
    (quota . ,(quota vm))
    (resolvers . ,(resolvers vm))
    ;; customer metadata
    ;; nics
    ;; filesystems
    ;; nics will need some work
    ;; !!! This should be able to be handled generally
    , (when (slot-boundp vm 'nics)
        `(nics . ,(mapcar #'json-spec (nics vm))))

    ;; filesystems
    ))


;; let's check if we have enough information to create a zone...
(defmethod create-plan :before ((vm smartos-zone))
  ;; just try and get this. It will fail if there are important missing things
  (json-spec vm))

(defmethod destroy ((vm smartos-zone))
  (execute-command (parent vm)
                   "vmadm"
                   (list "delete" (uuid vm))))

(defmethod create ((vm smartos-zone))
  ;; make sure this gets reloaded
  (slot-makunbound (parent vm) 'vms)
  (execute-command (parent vm)
                   "vmadm"
                   "create"
                   :input (json:encode-json-to-string (json-spec vm))
                   :output :first-line))




