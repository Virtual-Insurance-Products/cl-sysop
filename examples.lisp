
(defun ovh1 (&rest components)
  (make-instance 'smartos-host :name "ovh1"
                 :subcomponents components))


;; so if something goes wrong we can inspect results like this
(defparameter *forwarding*
  (adopt (ovh1)
         (make-instance 'smf-service
                        :name "ipv4-forwarding")))

(tail (service-log *forwarding*))
(current-state *forwarding*)


;; I need classes for zones to play with the ABEL zone - that will be useful
