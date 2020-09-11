
(in-package :cl-sysop)

;; smf services have a concept of instances
(defclass smf-service (service)
  ((instance :initform "default" :reader instance)
   (enabled :initform t :reader enabled :initarg :enabled)
   (current-state :reader current-state)
   (current-state-since :reader current-state-since)))

;; check whether the service exists

(defmethod exists-p ((svc smf-service)))

;; I can't really create this

(defmethod fmri ((x smf-service))
  (concatenate 'string (name x) ":" (instance x)))

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


;; one that I define
(defclass custom-smf-service (smf-service system)
  ((manifest :initarg :manifest :reader manifest)
   (control-script)
   (subcomponents)))

;; subcomponents will include the manifest file and control script as well as the other things
(defmethod subcomponents ((x custom-smf-service))
  (append (list (make-instance 'fs-file :full-path
                               :content (manifest x))
                )))

;; I can create this custom one though
;; the way of creating might be slightly different between SmartOS GZ and normal Solaris system
;; or it might not


