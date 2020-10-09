
(in-package :cl-sysop)

;; smf services have a concept of instances
(defclass smf-service (service)
  ((instance :initform "default" :reader instance)
   (fmri :initarg :fmri :accessor fmri)
   (enabled :initform t :reader enabled :initarg :enabled)
   (current-state :reader current-state)
   (current-state-since :reader current-state-since :initarg :current-state-since)))

;; check whether the service exists
(defmethod existing-services :before ((host solaris-host))
  (unless (slot-boundp host 'existing-services)
    (setf (slot-value host 'existing-services)
          (loop for line in (execute-command host
                                            "svcs" '("-H" "-a")
                                            :output :lines)
                for bits = (cl-ppcre:split "\\s+" line)
                collect (destructuring-bind (status since fmri)
                            bits
                          (make-instance 'smf-service
                                         :enabled status
                                         :current-state-since since
                                         :fmri fmri
                                         :name (cl-ppcre:regex-replace ":.*"
                                                                       (cl-ppcre:regex-replace ".*/" fmri "")
                                                                       "")))))))

(defmethod smf-service-exists-p ((svc smf-service))
  (if (slot-boundp svc 'fmri)
      (find (fmri svc)
            (existing-services (host svc))
            :test #'equal :key #'fmri)
      (let ((found (find (name svc)
                         (existing-services (host svc))
                         :test #'equal :key #'name)))
        (when found
          (setf (fmri svc)
                (fmri found))))))

;; It would be better to use the FMRI or the FMRI search facility, but this will suffice for the mo
;; !!! It would be good to get service properties instead of getting all existing services
;; (although this will speed up adding >1 service)
(defmethod exists-p ((svc smf-service))
  (smf-service-exists-p svc))

(defmethod fmri :before ((svc smf-service))
  (unless (slot-boundp svc 'fmri)
    ;; if we try and find it we should get the FMRI if possible
    (exists-p svc)))

;; this is helpful if we want to examine the service log
(defmethod service-log ((x smf-service))
  (adopt (host x)
         (make-instance 'fs-file
                        :full-path (execute-command (host x)
                                                    "svcs" (list "-L" (fmri x))
                                                    :output :first-line))))

(defmethod get-current-state ((svc smf-service))
  (destructuring-bind (state since fmri)
      (cl-ppcre:split "\\s+"
                      (execute-command (host svc)
                                       "svcs" (list "-H" (fmri svc))
                                       :output :first-line))
    (declare (ignore fmri))
    (setf (slot-value svc 'current-state) state
          (slot-value svc 'current-state-since) since)))

(defmethod current-state :before ((svc smf-service))
  (unless (slot-boundp svc 'current-state)
    (get-current-state svc)))

(defmethod current-state-since :before ((svc smf-service))
  (unless (slot-boundp svc 'current-state-since)
    (get-current-state svc)))

;; we need: clear, enable, disable
;; if the service goes into maintenance then we will attempt to clear it if enabled is requested
;; having done so we will check to see if it then goes online
;; if not we will throw an error after tailing the log
;; that will make diagnosing service defintion problems pretty convenient.

;; Afterwards I need to do something similar for lx brand zone services if I want to use those
;; (but I'd much rather put Traefik in a solaris zone)

(defmethod update-plan ((svc smf-service) &optional without)
  (declare (ignore without))
  ;; see if the service is running. If the running state doesn't agree with desired then we must change that
  (if (enabled svc)
      (let ((current (current-state svc)))
        (cond ((equal current "maintenance")
               (append (call-next-method)
                       `((clear ,svc)
                         (check ,svc))))
              ((equal current "disabled")
               (append (call-next-method)
                       `((enable ,svc)
                         (check ,svc))))))))

(defmethod create-plan ((svc smf-service))
  `((enable ,svc)
    (check ,svc)))

;; I don't know how this will happen in general unless something else has changed
;; it WOULD work with the gc-monitor though
(defmethod clear ((svc smf-service))
  (execute-command (host svc)
                   "svcadm"
                   (list "clear" (fmri svc))))

(defmethod check ((svc smf-service))
  (slot-makunbound svc 'current-state)

  ;; give it chance to start up
  (loop for current = (current-state svc)
        while (equal current "offline*")
        do (slot-makunbound svc 'current-state)
           (format t "Waiting for service to transition from offline* state...~%")
        (sleep 1))
  
  (let ((current (current-state svc)))
    (when (equal current "maintenance")
      (error (tail (service-log svc))))))

(defmethod enable ((svc smf-service))
  (execute-command (host svc)
                   "svcadm"
                   (list "enable" (fmri svc))))

(defmethod restart ((svc smf-service))
  (execute-command (host svc)
                   "svcadm"
                   (list "restart" (fmri svc))))

(defmethod refresh ((svc smf-service))
  (execute-command (host svc)
                   "svcadm"
                   (list "refresh" (fmri svc))))

;; I can create this custom one though
;; the way of creating might be slightly different between SmartOS GZ and normal Solaris system
;; or it might not




;; What do we need to get this online?
(defclass custom-smf-service (system smf-service)
  ((start-command :initarg :start-command :reader start-command)

   ;; the good thing is that because all these properties are just used to generate the script and manifest files
   ;; we don't have to handle change detection - the subcomponents will be those files and they will either match or not
   ;; this might be needed in smf-service
   (instances :initform '("default") :initarg :instances :reader instances)
   ;; maybe these things should be better modelled
   (dependencies :initform '(("network" "svc:/milestone/network:default")
                             ("filesystem-local" "svc:/system/filesystem/local:default"))
                 :reader dependencies)
   ;; extra properties etc
   ;; (which can be picked up by run scripts)
   (method-variables :initarg :method-variables :initform nil :reader method-variables)
   ))

;; (make-instance 'custom-smf-service)

;; !!! There must be a better way to stop the user of system's exists-p here. What though? 
(defmethod exists-p ((svc custom-smf-service))
  (smf-service-exists-p svc))

(defmethod subcomponents ((svc custom-smf-service))
  (when (slot-boundp svc 'parent)
    (mapcar (lambda (child)
              (adopt svc child))
            (list (service-method svc)
                  (service-manifest svc)))))

;; if the subcomponents (manifest, script etc) have changed then we have to rebuild
;; that does mean destroying and rebuilding the service which might not always be a Good Idea
;; I might revisit this, or change it in subclasses of this
(defmethod requires-rebuild-p ((svc custom-smf-service))
  (reduce #'append
          (mapcar #'update-plan
                  (subcomponents svc))))


;; if you want instance properties then subclass this to return some XML - I may find a better way later
(defmethod instance-properties ((svc custom-smf-service) instance)
  (declare (ignore instance))
  nil)

(defmethod custom-smf-path ((svc custom-smf-service) (host joyent-zone) (what (eql :method)))
  (format nil "/opt/local/lib/svc/method/~A" (name svc)))

(defmethod custom-smf-path ((svc custom-smf-service) (host smartos-host) (what (eql :method)))
  (format nil "/opt/custom/smf/~A" (name svc)))

;; !!! FIXME - make this more betterer
(defmethod service-method ((svc custom-smf-service))
  (make-instance 'fs-file :full-path (custom-smf-path svc (host svc) :method)
                          :permissions #o555
                          :content (concatenate 'string
                                                "#!/sbin/sh
#

. /lib/svc/share/ipf_include.sh
. /lib/svc/share/smf_include.sh

getproparg() {
   val=`svcprop -p $1 $SMF_FMRI`
   [ -n \"$val\" ] && echo $val
}

# 
INSTANCE=`echo $SMF_FMRI|sed 's/.*://'`
PIDFILE=/var/run/" (name svc) ".pid

" (with-output-to-string (stream)
    (loop for (name . value) in (method-variables svc)
          do (format stream "~A=~S~%" (symbol-name name) value))) "

case $1 in 
        # SMF arguments (start and restart [really \"refresh\"])


'start')
        " (start-command svc) " &
        ;;

'restart')
        if [ -f \"$PIDFILE\" ]; then
                /usr/bin/kill -HUP `/usr/bin/cat $PIDFILE`
        fi
        ;;
        

esac	

exit $?


")
                 ))



(defmethod custom-smf-path ((svc custom-smf-service) (host joyent-zone) (what (eql :manifest)))
  (format nil "/opt/local/lib/svc/manifest/~A.xml" (name svc)))

(defmethod custom-smf-path ((svc custom-smf-service) (host smartos-host) (what (eql :manifest)))
  (format nil "/opt/custom/smf/~A.xml" (name svc)))


(defmethod service-manifest ((svc custom-smf-service))
  (make-instance
   'fs-file
   :full-path (custom-smf-path svc (host svc) :manifest)
   :permissions #o555
   :content
   ;; I should use an XML generator here really
   ;; I should also make dependencies explicity. Really it would be good to properly generate this file and look at the whole config
   (concatenate 'string
                "<?xml version=\"1.0\"?>
<!DOCTYPE service_bundle SYSTEM \"/usr/share/lib/xml/dtd/service_bundle.dtd.1\">"
                (xmls:toxml
                 `("service_bundle"
                   (("type" "manifest")
                    ("name" ,(concatenate 'string
                                          "application/"
                                          (name svc))))
                   ("service" (("name" ,(name svc))
                               ("type" "service")
                               ("version" "1"))
                              ,@ (mapcar (lambda (dep)
                                           ;; for now I'm just putting these in like this
                                           (destructuring-bind (name fmri)
                                               dep
                                             `("dependency" (("type" "service")
                                                             ("restart_on" "none")
                                                             ("grouping" "require_all")
                                                             ("name" ,name))
                                                            ("service_fmri" (("value" ,fmri))))))
                                         (dependencies svc))
                                 ;; these could all be parameterised
                              ("exec_method" (("timeout_seconds" "60")
                                              ("exec" ,(format nil "~A start"
                                                               (custom-smf-path svc (host svc) :method)))
                                              ("name" "start")
                                              ("type" "method")))

                              ("exec_method" (("timeout_seconds" "60")
                                              ("exec" ,(format nil "~A restart"
                                                               (custom-smf-path svc (host svc) :method)))
                                              ("name" "refresh")
                                              ("type" "method")))

                              ("exec_method" (("timeout_seconds" "60")
                                              ("exec" ":kill")
                                              ("name" "stop")
                                              ("type" "method")))

                              ;; now create the service instances
                              ;; Ideally we should be able to control whether each is enabled or not...
                              ,@ (mapcar (lambda (instance)
                                           `("instance" (("enabled" "true")
                                                         ("name" ,instance))))
                                         (instances svc))

                              ("template" nil
                                          ("common_name" nil
                                                         ("loctext" (("xml:lang" "C"))
                                                                    ,(name svc))))))))))

;; I'm overriding this to create the subcomponents FIRST as opposed to the usual order
(defmethod create-plan ((svc custom-smf-service))
  (append (reduce #'append
                  (mapcar #'create-plan (subcomponents svc)))
          `((create ,svc))))

;; as soon as it is imported it should start right up
(defmethod create ((svc custom-smf-service))
  (execute-command (host svc)
                   "svccfg"
                   (list "import"
                         (custom-smf-path svc (host svc) :manifest)))
  (slot-makunbound (host svc) 'existing-services)
  (check svc))

;; https://blogs.oracle.com/solaris/changes-to-svccfg-import-and-delete says I shouldn't use svccfg delete
;; but I'm not sure if that's applicable to SmartOS. It seems to work anyway
(defmethod destroy ((svc custom-smf-service))
  (execute-command (host svc)
                   "svcadm"
                   (list "disable" (fmri svc)))
  (execute-command (host svc)
                   "svccfg"
                   (list "delete"
                         :f ; forcibly so we can delete online services. This is a bit harsh - would be better not to do this
                         (fmri svc))))

