
(in-package :cl-sysop)

;; General thing for requiring a certain binary to be installed
;; !!! I should handle required versions

(defclass installed-binary (named component)
  ((source-package :initarg :source-package :reader source-package)))

(defmethod exists-p ((b installed-binary))
  (handler-case
      (execute-command (host b)
                       "which" (name b)
                       :output :first-line)
    (shell-error ()
      nil)))

;; (exists-p (adopt (localhost) (make-instance 'installed-binary :name "unzip")))
;; (exists-p (adopt (liganc) (make-instance 'installed-binary :name "unzip")))

;; This isn't guaranteed to work of course, but it might
;; if it doesnt' then we might have to subclass installed binary
(defmethod create-plan ((b installed-binary))
  (create-plan (adopt (host b)
                      ;; !!! Maybe it would be better if source-package was a package so I could specify other things too
                      (make-instance 'software-package
                                     :name (if (slot-boundp b 'source-package)
                                               (source-package b)
                                               (name b))))))
