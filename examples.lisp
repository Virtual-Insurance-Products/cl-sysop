



;; so if something goes wrong we can inspect results like this
(defparameter *forwarding*
  (adopt (ovh1)
         (make-instance 'smf-service
                        :name "ipv4-forwarding")))

(tail (service-log *forwarding*))
(current-state *forwarding*)


;; I need classes for zones to play with the ABEL zone - that will be useful




(mapcar #'alias (vms (ovh2)))
(defparameter *app-vm* (find "dtw-application-vm" (vms (ovh2)) :test 'equal :key 'alias))

(vmadm-get *app-vm*)

;; now I can execute commands in a zone. That's rather useful
(execute-command *app-vm* "uname" "-a")


(defun traefik-server (host &rest subcomponents)
  (find "traefik-server" (vms host) :test 'equal :key 'alias))

(defparameter *traefik* (traefik-server))

(ovh1
 (make-instance 'lx-zone
                :alias "traefik-server"
                ))

;; now I can describe files in that zone
(exists-p (adopt (ovh1)
                 (make-instance 'lx-zone
                                :alias "traefik-server"
                                :subcomponents)))

;; next we need service setup and downloading/installing the traefik service.
;; if I can get this whole vm described I'd like to rebuild it like this. I'd better not though
;; I should work on abel instances


;; now, the thing is: it's going to be a certain amount of work to get all these bits in play so that all of this works.
;; it will be good once it is.
;; if I want to change the services listed above it will be easy to do so and they will just be blatted into place.

(existing-content (adopt (adopt (ovh1)
                                (make-instance 'lx-zone
                                               :alias "traefik-server"
                                               ))
                         (make-instance 'fs-directory
                                        :full-path "/etc/traefik"
                                        :content nil
                                        )))



(existing-content (adopt (localhost)
                         (make-instance 'fs-directory :full-path "/etc" :content nil)))

;; SO I can now get existing content. I could do that recursively. I should implement it for files as well read the file
;; but sometimes we'll want to work in terms of hashes.
;; Why was I doing this?
;; Ideally I want to implement exists-p properly
;; existing content will also let me remove things we DON'T want, which will be needed here


;; It would be good to make some OS X service stuff so I can put
;; consul into an OSX service. I could even run the RSS reader like
;; that.
