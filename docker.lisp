
(in-package :cl-sysop)


;; Allow running of docker images on hosts which can run docker images
;; I need to describe a docker image
;; can I or do I allow them to be treated as normal unix hosts - ie log into them etc?
;; I'm not sure.

;; Although I'll be able to run docker containers on things OTHER THAN SmartOS, they need to inherit the properties of an lx zone
;; so that we can build them...
(defclass docker-container (lx-zone)
  ((json-property::docker :initform t)
   (requires-rebuild-p :initarg :rebuild :reader requires-rebuild-p :initform nil)
   (docker-image :initarg :docker-image :reader docker-image)
   (docker-cmd :initarg :docker-cmd :reader docker-cmd
               ;; maybe this is useful and a convention?
               :initform (list "/entrypoint.sh")
               )))

;; image-uuid is requested before generating the json spec
(defmethod image-uuid :before ((vm docker-container))
  (unless (slot-boundp vm 'json-property::image_uuid)
    (setf (slot-value vm 'json-property::image_uuid)
          (cdr (find :uuid (json:decode-json-from-string
                            (execute-command (host vm)
                                             "imgadm"
                                             (list "show" (docker-image vm))))
                     :key 'first)))))

(defmethod create-plan ((vm docker-container))
  (let ((next (loop for (a . r) in (call-next-method)
                    collect (if (eq a 'import-image)
                                (list 'import-image (first r) (docker-image vm))
                                (cons a r)))))
    
    (if (find "https://docker.io"
              (execute-command (host vm)
                               "imgadm" "sources"
                               :output :lines)
              :test #'equal)
        next
        (append `((add-docker-hub ,(host vm)))
                next))))

;; !!! Move to examples.lisp
;; This is looking good though
(when nil
  (json:encode-json-to-string
   (json-spec
    (adopt (liganc)
           (make-instance 'docker-container
                          :docker-image "busybox"
                          :alias "norbert")))))

;; before getting the json-spec we need to find the image-uuid of the requested docker image. 

(defmethod json-spec ((vm docker-container))
  (if (slot-boundp vm 'docker-cmd)
      (append (call-next-method)
              `((:internal_metadata . ,(vip-utils:hash (list "docker:cmd"
                                                             (json:encode-json-to-string (docker-cmd vm)))))))
      (call-next-method)))

(defun docker-volume (source target)
  (smartos-filesystem source target))
