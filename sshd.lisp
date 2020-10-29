
(in-package :cl-sysop)

;; I need to start building out SSH certificate auth infrastructure
;; here It will be great to be able to manage this.  I guess there's a
;; bit of a chicken and egg situation here - I would essentially have
;; to use passwords(?) initially to get access and then
;; reconfigure. Well, I can allow access into zones w/o that


(defun ssh-identity-type-p (x)
  (member x '("ed25519" "rsa") :test #'equal))

;; this is an abstract base which can be implemented by various things...
(defclass ssh-certificate-authority () ())

;; an SSH identity is an acceptable CA
(defclass ssh-identity (ssh-certificate-authority)
  ((private-key :initarg :private-key :reader private-key)
   (public-key :initarg :public-key :reader public-key)
   ;; Should I default this?
   (type
    :initarg :type :reader type
    :type (and string (satisfies ssh-identity-type-p)))))

(defmethod initialize-instance :after ((x ssh-identity) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp x 'public-key)
    (unless (slot-boundp x 'type)
      (error "Specify :type of \"ed25519\" or \"rsa\""))
    (in-temporary-directory nil
      (execute-command (localhost)
                       "ssh-keygen"
                       (list "-t" (type x) "-f" "id"))
      (setf (slot-value x 'private-key)
            (existing-content (adopt (localhost)
                                     (make-instance 'fs-file
                                                    :full-path "id")))
            (slot-value x 'public-key)
            (existing-content (adopt (localhost)
                                     (make-instance 'fs-file
                                                    :full-path "id.pub"))))))

  (when (and (slot-boundp x 'public-key)
             (not (slot-boundp x 'type)))
    (setf (slot-value x 'type)
          (cl-ppcre:regex-replace "^ssh-"
                                  (first (cl-ppcre:split "\\s+" (public-key x)))
                                  ""))))


;; Maybe this is an easier way to generate an identity - don't need the generator function
;; (make-instance 'ssh-identity :type "ed25519")
;; (make-instance 'ssh-identity :type "rsa")
;; (make-instance 'ssh-identity :type "dsa")
;; This errors:-
;; (make-instance 'ssh-identity)

;; Can also just initialize with a public key:-
;; (make-instance 'ssh-identity :public-key "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAuiwjjB6+3PZyBrDohMdxi1qnQn8Dc4yZFKoWf8zaMr1JM0j/AVVhrfHFeQYFu7STSLFQhRMe8jLSQaqvor0u6K37lpVd8WAgeTBlHnwRedSPkuJ2H6hoPxZ7TcJRiHSQBcMdtNbhq+J/cY8VoWg54MsF+YWzacdsYcRkpiZv93F/91fXAn2OLGWfxeXcPawZGotLk4y68GFxAOYDICwr/GOVO2GxarwfoG8tXFdYuyBmF/6iLuF7VYjW0cVj/teMW0LVW7SHrX38RpPLpZbwUsYK+z1/+UaFNEGX2gNKUpSCN2d1GEBXb2r9863aMDn5nRn4OI48EZDqMRSFEu0KCw== david@legion.local")

(defmethod creation-expression ((id ssh-identity))
  (cond ((and (slot-boundp id 'private-key)
              (slot-boundp id 'public-key))
         `(make-instance ',(class-name (class-of id))
                         :private-key ,(private-key id)
                         :public-key ,(public-key id)
                         :type ,(type id)))
        ((slot-boundp id 'public-key)
         `(make-instance ',(class-name (class-of id))
                         :public-key ,(public-key id)))
        (t nil)))

(defmethod print-object ((id ssh-identity) (s stream))
  (if (creation-expression id)
      (format s "~S" (creation-expression id))
      (call-next-method)))

;; (make-ssh-identity)


;; now declare that some public key should be trusted

;; This certificate is not the same as an openssl certificate, though it is similar. 
(defclass signed-ssh-identity (ssh-identity)
  ((certificate :initarg :certificate :reader certificate)
   (info :reader ssh-certificate-info)))

(defmethod creation-expression ((x signed-ssh-identity))
  (append (call-next-method)
          (list :certificate (certificate x))))

;; container will normally be a fs-directory
(defun read-ssh-identity (name container)
  (flet ((piece (name)
           (existing-content (adopt container
                                    (make-instance 'fs-file
                                                   :parent container
                                                   :name name)))))
    (if (exists-p (adopt container
                         (make-instance 'fs-file :name
                                        (concatenate 'string name "-cert.pub"))))
        (make-instance 'signed-ssh-identity
                       :private-key (piece name)
                       :public-key (piece (concatenate 'string name ".pub"))
                       :certificate (piece (concatenate 'string name "-cert.pub")))
        (make-instance 'ssh-identity
                       :private-key (piece name)
                       :public-key (piece (concatenate 'string name ".pub"))))))



;; This isn't quite as flexible as what SSH allows, which is mixed units
;; I don't think it matters
(deftype ssh-time-format (&optional (range 'integer))
  `(cons ,range
         (cons (member :second :seconds
                       :minute :minutes
                       :hour :hours
                       :day :days
                       :week :weeks))))

;; (typep '(1 :hour) 'ssh-time-format)
;; (typep '(1 :fortnight) 'ssh-time-format)
;; (typep '(10 :minutes) 'ssh-time-format)
;; (typep :always '(ssh-time-format))

;; I'll make this explicit and picky. I could declare a class for validity interval with a print etc
;; it just seems easier to use the simple notation below. Maybe it's not. 
(defun validity-interval (valid-from valid-to)
  ;; does SSH allow certs which will become valid in the future? It doesn't seem to say so
  (check-type valid-from (or integer    ; a universal time
                             (ssh-time-format (integer -1000000000 -1)) (eql :always)))
  (check-type valid-to (or integer
                           (ssh-time-format (integer 1)) (eql :forever)))
  (flet ((f (x)
           (cond ((symbolp x)
                  x)
                 ((integerp x)
                  (multiple-value-bind (second minute hour day month year)
                      (decode-universal-time x)
                    (format nil "~A~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D" year month day hour minute second)))
                 (t (format nil "~A~A" (first x) (elt (symbol-name (second x)) 0))))))
    (format nil "~(~A:+~A~)"
            (f valid-from)
            (f valid-to))))

;; (validity-interval '(-1 :week) '(0 :weeks))
;; (validity-interval '(-1 :week) :forever)
;; (validity-interval :always (+ (get-universal-time) 10000))

;; Ideally I need to be able to inspect certificates too. Probably
;; ssh-keygen has some options to do that.

;; sign one ssh id with another
;; we only require the public key of the identity to sign 
(defmethod sign-ssh-identity ((identity-to-sign ssh-identity) (signing-identity ssh-identity)
                              &key host-p (identity (error ":identity required"))
                                (valid-from :always)
                                (valid-to :forever)
                                principals
                                (serial-number (get-universal-time))
                                )
  (let ((interval (validity-interval valid-from valid-to)))
    (in-temporary-directory
        (list (make-instance 'fs-file
                             :name "id.pub"
                             :content (public-key identity-to-sign))
              (make-instance 'fs-file
                             :name "ca"
                             :permissions #o400
                             :content (private-key signing-identity))
              (make-instance 'fs-file
                             :name "ca.pub"
                             :content (public-key signing-identity)))
      
      (execute-command (localhost)
                       "ssh-keygen"
                       `( "-s" "ca"
                               "-I" ,identity
                               ,@(when host-p
                                   (list "-h"))     
                               ,@ (when principals
                                    (list "-n"
                                          (format nil "~{~a~^,~}" principals)))
                               ,@ (when serial-number (list "-z" serial-number))
                               "-V" ,interval
                               "id.pub"))
    
      (change-class identity-to-sign 'signed-ssh-identity
                    :certificate (existing-content
                                  (adopt (localhost)
                                         (make-instance 'fs-file
                                                        :full-path "id-cert.pub"))))
      ;; in case we're re-signing
      (slot-makunbound identity-to-sign 'info)
      identity-to-sign)))

;; (defmethod trust-key)

;; It can be resigned too
;; (sign-ssh-identity *ssh-id* *ssh-ca* :identity "root" :valid-to (+ 600 (get-universal-time)))
;; (sign-ssh-identity *ssh-id* *ssh-ca* :identity "John Smith" :valid-to '(5 :minutes) :principals (list "root"))
;; (sign-ssh-identity *ssh-id* *ssh-ca* :identity "root" :valid-to '(5 :fortnights))
;; (sign-ssh-identity *ssh-id* *ssh-ca* :host-p t :identity "ovh1" :valid-to '(5 :weeks))

;; ok - nice and explicit

;; how do I parse the certificate info? It's not a complicated format to parse
(defmethod ssh-certificate-info :before ((x signed-ssh-identity))
  (unless (slot-boundp x 'info)
    (setf (slot-value x 'info)
          (labels ((trim (x)
                     (setf x (cl-ppcre:regex-replace "^\\s+" x ""))
                     (setf x (cl-ppcre:regex-replace "\\s+$" x ""))
                     x)

                   (key-symbol (x)
                     (intern (string-upcase (cl-ppcre:regex-replace-all "\\s+" x "-"))
                             :keyword))
           
                   (parse (lines)
                     (when lines
                       (let ((first (first lines)))
                         (if (equal (cl-ppcre:regex-replace ".*?\\:\\s+" first "")
                                    "")
                             ;; multi line ones
                             (let ((key (trim (cl-ppcre:regex-replace "\\:\\s*$" first "")))
                                   (value nil))
                               (loop while (and (cdr lines)
                                                (not (cl-ppcre:scan "\\:" (second lines))))
                                     do
                                        (setf lines (cdr lines))
                                        (push (trim (first lines)) value))
                               (cons (cons (key-symbol key) (reverse value))
                                     (parse (cdr lines))))
                             ;; single line
                             (cons (let ((key (trim (cl-ppcre:regex-replace "\\:.*" first "")))
                                         (value (cl-ppcre:regex-replace "^.*?\\:\\s*" first "")))
                                     (cons (key-symbol key)
                                           (if (equal key "Key ID")
                                               (cl-ppcre:regex-replace-all "\"" value "")
                                               value)))
                                   (parse (cdr lines))))))))
    
            (parse (cdr (execute-command (localhost)
                                         "ssh-keygen"
                                         (list "-L" "-f" "-")
                                         :input (certificate x)
                                         :output :lines)))))))

(defmethod ssh-certificate-property ((x signed-ssh-identity) property)
  (cdr (assoc property (ssh-certificate-info x))))

;; then we can get things like
(defmethod principals ((x signed-ssh-identity)) (ssh-certificate-property x :principals))
;; (principals *ssh-id*)

(defmethod validity ((x signed-ssh-identity))
  (mapcar #'cybertiggyr-time:parse-time
          (cl-ppcre:split " to "
                          (cl-ppcre:regex-replace "from "
                                                  (ssh-certificate-property x :valid) ""))))

(defmethod valid-p ((x signed-ssh-identity))
  (destructuring-bind (from to)
      (validity x)
    (format t "VALID: ~A ~A ~A ~%" from (get-universal-time) to)
    (and (>= (get-universal-time) from)
         (<= (get-universal-time) to))))


;; NOW, if we have to have the CA key stored on the local machine
;; that's not very secure, so instead let's implement some OTHER
;; things to sign ssh keys

;; The first I'll implement is achieved by forcing execution of an SSH
;; command on an ssh host in response to a certain key. Password login
;; should be disabled.

;; For the moment this will have to be manuall initialised with the public key
(defclass ssh-accessible-ca (sshd-host ssh-certificate-authority)
  ((public-key :initarg :public-key :reader public-key)))

;; then we can implement this...
(defmethod sign-ssh-identity ((identity ssh-identity) (ca ssh-accessible-ca) &rest args)
  (declare (ignore args))
  ;; basically it's up to that CA as to /how/ it signs since I have no way to even ask for args
  ;; in general though, it will probably give me short but powerful certificates
  (let ((cert (execute-command ca
                               "echo"   ; ignored
                               nil
                               :input (public-key identity))))
    (change-class identity 'signed-ssh-identity
                  :certificate cert)
    (slot-makunbound identity 'info)
    identity))


;; I could also make a vault ca - that would probably be useful



;; Now we have to create another class which can be used as a component.
;; This makes SSH keys for us
;; This is similar to rsa-certificate-pair and its subclass for consul
;; I can't use those (productively) though since the generation and validation (more importantly) is different
(defclass ssh-key-pair (system fs-object)
  ((certificate-authority :initarg :certificate-authority :type ssh-certificate-authority
                          :reader certificate-authority)
   (ssh-identity :accessor ssh-identity :type ssh-identity)
   (principals :initarg :principals :reader principals :type list)
   (valid-from :initarg :valid-from)
   (valid-to :initarg :valid-to))
  (:default-initargs :name "id"))

(defmethod create ((x ssh-key-pair))
  (setf (ssh-identity x)
        (make-instance 'ssh-identity :type "ed25519"))

  (when (slot-boundp x 'certificate-authority)
    (sign-ssh-identity (ssh-identity x)
                       (certificate-authority x)))
  
  x)

;; How do I specify the content
(defmethod subcomponents ((x ssh-key-pair))
  (when (slot-boundp x 'parent)
    (mapcar (adopter (parent x))
            
            `(,(make-instance 'fs-file
                              :permissions #o600
                              :name (name x)
                              ;; I have to thunk all of these in case we haven't created it yet
                              :content (lambda ()
                                         (private-key (ssh-identity x))))

              ;; I think I probably don't need this if there's a
              ;; certificate if an identity is supplied then we
              ;; can just sign it, but there isn't much point
              ;; really - we may as well just generate a new
              ;; identity each time. That way private key theft
              ;; won't matter after the certificate expires
              ,(make-instance 'fs-file
                              :name (concatenate 'string (name x) ".pub")
                              :content (lambda ()
                                         (public-key (ssh-identity x))))

              ;; Only if we have a CA, which we don't have to have
              ,@(when (slot-boundp x 'certificate-authority)
                  (list
                   (make-instance 'fs-file
                                  :name (concatenate 'string (name x) "-cert.pub")
                                  :content (lambda ()
                                             (certificate (ssh-identity x))))))
                  
              ))))

;; existence is easy enough
(defmethod exists-p ((pair ssh-key-pair))
  (not (member nil
               (mapcar #'exists-p
                       (subcomponents pair)))))

(defmethod requires-rebuild-p ((x ssh-key-pair))
  ;; if we get here we assume that the files exist
  ;; if they exist then we can't really verify the private and pub keys if there's no CA
  ;; - they're all equally as good.
  ;; BUT if there's a CA...
  (when (slot-boundp x 'certificate-authority)
    (let ((existing (read-ssh-identity (name x) (parent x))))
      (not (and (typep existing 'signed-ssh-identity)
                (valid-p existing)
                (every (lambda (p)
                         (find p (principals existing) :test #'equal))
                       (principals x)))))))

;; If they don't need creating or rebuilding then we're good
(defmethod update-plan ((x ssh-key-pair) &optional without)
  (when without
    (call-next-method)))

;; really nothing to do here. Maybe I should replace the destroy plan
(defmethod destroy ((x ssh-key-pair)))



;; Examine sshd_config on a server to see whether it trusts any CAs and whether a given one is included...

(defmethod sshd-config-file ((x unix-host))
  (adopt x (make-instance 'fs-file :full-path "/etc/ssh/sshd_config")))

(defmethod sshd-config-file ((x smartos-host))
  (adopt x (make-instance 'fs-file :full-path "/usbkey/ssh/sshd_config")))

(defmethod sshd-ca-file ((x unix-host) content)
  (adopt x (make-instance 'fs-file :full-path "/etc/ssh/CA.pub" :content (format nil "~A~%" content))))

(defmethod sshd-ca-file ((x smartos-host) content)
  (adopt x (make-instance 'fs-file :full-path "/usbkey/ssh/sshd-CA.pub" :content (format nil "~A~%" content))))

;; (existing-content (sshd-config-file (localhost)))

;; Should I turn config params to keywords? It is handy
(defmethod sshd-config ((x unix-host))
  (mapcar (lambda (line)
            (let ((key (cl-ppcre:regex-replace "\\s+.*" line ""))
                  (value (cl-ppcre:regex-replace ".*?\\s+" line "")))
              (cons key value)))
          (remove-if (lambda (x)
                       (or (not (cl-ppcre:scan "[^\\s]" x))
                           (cl-ppcre:scan "^\\s*\\#" x)))
                     (cl-ppcre:split "\\n" (existing-content (sshd-config-file x))))))

;; (sshd-config (localhost))
;; (sshd-config (ovh1))


;; sshd configuration. This is sparse, so existing configuration
(defclass sshd-configuration (component)
  ((allow-other-options-p :initarg :allow-other-options-p :initform t :reader allow-other-options-p)
   (trust-certificate-authority :initarg :trust-certificate-authority :initarg :trust-ca
                                :type ssh-certificate-authority
                                :reader trust-certificate-authority)))

;; Let's assume that /some/ configuration exists
(defmethod exists-p ((x sshd-configuration)) t)

;; So, let's work out what updates are required.
;; This requires examining slots which are ssh configuration parameter names
;; AND, importantly, this bit...
(defmethod update-plan ((x sshd-configuration) &optional without)
  (when without
    (error "sshd config removal not yet implemented"))
  (when (slot-boundp x 'trust-certificate-authority)
    (let* ((existing (sshd-config (host x)))
           (ca-key-file (cdr (find "TrustedUserCAKeys" existing :key #'car :test #'equal)))
           (ca-file (sshd-ca-file (host x) (public-key (trust-certificate-authority x)))))

      (if ca-key-file
          ;; now we have to look in the CA file to see if it trusts this public key...
          (let ((trusted (cl-ppcre:split "\\n"
                                         (existing-content (adopt (host x)
                                                                  (make-instance 'fs-file
                                                                                 :full-path ca-key-file))))))
            (if (find (public-key (trust-certificate-authority x))
                      trusted
                      :test #'equal)
                ;; nothing to do
                nil
                `((append-file (adopt (host x)
                                      (make-instance 'fs-file
                                                     :full-path ca-key-file
                                                     :content (format nil "~A~%"
                                                                      (public-key (trust-certificate-authority x)))))))
                ))
          `((append-file ,(let ((file (sshd-config-file (host x))))
                            (setf (slot-value file 'content)
                                  (format nil "~%TrustedUserCAKeys ~A~%"
                                          (full-path ca-file)))
                            file))
            (create ,ca-file))))))
