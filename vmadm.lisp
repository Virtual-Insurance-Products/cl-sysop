
(in-package :cl-sysop)


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

