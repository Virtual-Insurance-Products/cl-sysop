
(in-package :cl-sysop)

;; !!! I haven't taken care of importing the images as needed in this. What to do? 

;; obviously there's a lot more information we could get here using
(defmethod vms :before ((host smartos-host))
  (unless (slot-boundp host 'vms)
    (setf (slot-value host 'vms)
          (let ((fields '(vmadm::uuid vmadm::type vmadm::ram vmadm::state vmadm::alias
                          vmadm::customer_metadata.source_uuid vmadm::image_uuid
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
  (loop for identifying-slot in '(vmadm::alias)
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
  (unless (slot-boundp host 'vmadm::uuid)
    ;; we need a way to find the zone described
    ;; we will look for certain slots in order...
    (when (exists-p host)
      (setf (slot-value host 'vmadm::uuid)
            (or (uuid (exists-p host))
                (error "Zone not found ~A" host))))))

(defparameter +base-64-feb-2020+ "ad6f47f2-c691-11ea-a6a5-cf0776f07bb7")

;; I should really just name these slots according to the json name
(defclass smartos-nic (vmadm-json-object)
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
(defmethod json-spec ((x vmadm-json-object))
  (loop for slot in (ccl:class-slots (class-of x))
        for name = (ccl:slot-definition-name slot)
        for value = (when (slot-boundp x name)
                      (slot-value x name))
        when (and (equal (package-name (symbol-package name)) "VMADM")
                  (slot-boundp x name))
          collect (cons name
                        (if (eq (ccl:slot-definition-type slot)
                                'object-list)
                            (mapcar #'json-spec value)
                            (slot-value x name)))))

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
(defmethod json-spec :before ((vm smartos-zone))
  (image-uuid vm) ; there needs to be one of these
  )

;; let's check if we have enough information to create a zone...
(defmethod create-plan :before ((vm smartos-zone))
  ;; just try and get this. It will fail if there are important missing things
  (json-spec vm))

(defmethod destroy ((vm smartos-zone))
  (execute-command (parent vm)
                   "vmadm"
                   (list "delete" (uuid vm))))

(defmethod json-spec ((vm smartos-zone))
  (append `((:hostname . ,(if (slot-boundp vm 'name)
                              (name vm)
                              (alias vm))))
          (call-next-method)))


(defmethod create ((vm smartos-zone))
  ;; make sure this gets reloaded
  (slot-makunbound (parent vm) 'vms)
  (execute-command (parent vm)
                   "vmadm"
                   "create"
                   :input (json:encode-json-to-string (json-spec vm))
                   :output :first-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Now, for VM update plans we have to examine the current state of the VM
;; If it doesn't exist that will have been detected, so this assumes that it does

;; !!! I think I should try and abstract out this pattern somehow
(defmethod current-specification :before ((vm smartos-zone))
  (unless (slot-boundp vm 'current-specification)
    (setf (slot-value vm 'current-specification)
          (let ((json:*json-symbols-package* (find-package "VMADM"))
                (json:*json-identifier-name-to-lisp* #'string-upcase))
            (json:decode-json-from-string
             (execute-command (host vm)
                              "vmadm" (list "get" (uuid vm))))))))

(defmethod requires-rebuild-p ((vm smartos-zone))
  (when (slot-boundp vm 'vmadm::image_uuid)
    (not (equal (image-uuid vm)
                (cdr (assoc 'vmadm::image_uuid
                            (current-specification vm)))))))


(defmethod vm-property-changed-p ((vm smartos-zone) property)
  ;; if the slot is unbound we don't care
  (and (slot-boundp vm property)
       (not (equal (slot-value vm property)
                   (cdr (assoc property
                               (current-specification vm)))))))

;; we'll just lie about this
;; SO this has to be handled specially
(defmethod vm-property-changed-p ((vm smartos-zone) (property (eql 'vmadm::nics)))
  nil)


(defmethod update-plan ((vm smartos-zone) &optional without)
  (if without
      (call-next-method)
      (append (loop for slot in (ccl:class-direct-slots (find-class 'smartos-zone))
                    for name = (ccl:slot-definition-name slot)
                    for writer = (first (ccl:slot-definition-writers slot))
                    for reader = (first (ccl:slot-definition-readers slot))
                    when (and (equal (package-name (symbol-package name)) "VMADM")
                              (vm-property-changed-p vm name))
                      collect (if writer
                                  `(setf (,(second writer) ,vm) ,(funcall reader vm))
                                  (return-from update-plan
                                    (append (destroy-plan vm)
                                            (create-plan vm)))))
              ;; set the VM properties AND THEN do all the other bits
              (call-next-method))))


;; This just stops destroying the subcomponents - there's no need
(defmethod destroy-plan ((vm smartos-zone))
  `((destroy ,vm)))


;; now we can define behaviours for the things which /can/ be changed
;; because these are created as writers you /can/ just change these in 'direct mode'
;; - grab a reference to the VM and call the appropriate setf method

(defmethod (setf max-physical-memory) :after (new (vm smartos-zone))
  ;; clear this since it will be out of date
  (slot-makunbound vm 'current-specification)
  (execute-command (parent vm)
                   "vmadm" (list "update"
                                 (uuid vm))
                   :input (json:encode-json-to-string (list (cons 'vmadm::max_physical_memory
                                                                  new)))))

