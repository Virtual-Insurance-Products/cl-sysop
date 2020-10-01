
(in-package :cl-sysop)

;; I'm sure there are existing CL libraries to do these things
;; maybe I should use them but I've found openssl commands to do what I want, so let's start with this

(defun make-rsa-key (&key (bits 4096))
  (execute-command (localhost)
                   "openssl" (list "genrsa" bits)))

;; (make-rsa-key)

;; This cert can be used as a CA, but you need the key to do the signing
(defun make-rsa-certificate (private-key &key (days 1024)
                                           (country "GB")
                                           (state "DV")
                                           (org "ACME")
                                           (common-name "example.com"))
  (execute-command (localhost)
                   "openssl"
                   (list "req" :x509
                         :new :nodes
                         :key "/dev/stdin"
                         :subj (format nil "/C=~A/ST=~A/O=~A/CN=~A"
                                       country state org common-name)
                         :sha256
                         :days days)
                   
                   :input private-key))

;; Obviously you have to save the key!
;; (make-rsa-certificate (make-rsa-key))

(defun make-rsa-certificate-signing-request (private-key &key (days 1024)
                                                           (country "GB")
                                                           (state "DV")
                                                           (org "VIP")
                                                           (common-name "example.com"))
  (execute-command (localhost)
                   "openssl"
                   (list "req" :new :sha256
                         :key "/dev/stdin"
                         :subj (format nil "/C=~A/ST=~A/O=~A/CN=~A"
                                       country state org common-name)
                         :sha256
                         :days days)
                   
                   :input private-key))

;; (make-rsa-certificate-signing-request (make-rsa-key))

(defun sign-certificate-request (csr ca ca-key)
  (with-temporary-resources
      ((csr (temporary-file csr))
       (ca (temporary-file ca))
       (ca-key (temporary-file ca-key))
       (cert (temporary-file "")))
    (execute-command (localhost)
                     "openssl"
                     (list "x509" :req :in (full-path csr)
                           "-CA" (full-path ca)
                           "-CAkey" (full-path ca-key)
                           "-CAcreateserial"))))



;; SO, the complete sequence of steps is:-
;; (defparameter *ca-key* (make-rsa-key))
;; (defparameter *ca* (make-rsa-certificate *ca-key*))
;; (defparameter *host-key* (make-rsa-key))
;; (defparameter *host-csr* (make-rsa-certificate-signing-request *host-key* :common-name "db.abelonline.co.uk"))
;; (sign-certificate-request *host-csr* *ca* *ca-key*)

;; NOW, in order to have these things persist we will have to evaluate things into the debugger to get the key and cert
;; we can make and sign a host key in one go - that's not a problem:-

(defun signed-host-key (ca ca-key &key (common-name "example.com"))
  (let* ((key (make-rsa-key))
         (csr (make-rsa-certificate-signing-request key :common-name common-name)))
    (values key (sign-certificate-request csr ca ca-key))))

;; (signed-host-key *ca* *ca-key* :common-name "bwdangi.local")


;; this will solve our rekeying problem - we won't recreate an existing cert if it's valid with the CA given
;; If no CA key is given then creation is not possible

;; NOW let's make a subclass of fs-file for storing certificates
(defclass rsa-certificate-pair (system fs-object)
  ((certificate-authority :initarg :certificate-authority
                          :initarg :ca
                          :accessor certificate-authority)
   (certificate-authority-key :initarg :certificate-authority-key
                              :initarg :ca-key
                              :accessor certificate-authority-key)
   (files :accessor subcomponents)
   (common-name :initarg :common-name :accessor common-name)
   ))

(defmethod adopt :after ((parent rsa-certificate-pair) (part fs-file))
  (when (slot-boundp parent 'full-path)
    (setf (full-path part)
          (concatenate 'string
                       (full-path parent)
                       "." (name part)))))

(defmethod (setf full-path) :after (value (pair rsa-certificate-pair))
  (declare (ignore value))
  (dolist (c (subcomponents pair))
    (adopt pair c)))

(defmethod subcomponents :before ((pair rsa-certificate-pair))
  (unless (slot-boundp pair 'files)
    (setf (subcomponents pair)
          (list (make-instance 'fs-file
                               :name "cert")
                (make-instance 'fs-file
                               :name "key"
                               :permissions #o600)))))

;; This makes exclude-others recognize the components of this pair
(defmethod component-p ((a fs-file) (pair rsa-certificate-pair))
  (find a (subcomponents pair) :test #'component-p))

;; check that the 2 components exist
(defmethod exists-p ((pair rsa-certificate-pair))
  (not (member nil
               (mapcar #'exists-p
                       (subcomponents pair)))))

(defmethod part ((pair rsa-certificate-pair) part)
  (find (concatenate 'string (full-path pair)
                     "." part)
        (subcomponents pair)
        :test #'equal :key #'full-path))

(defmethod get-existing-field ((pair rsa-certificate-pair) &optional (field "CN"))
  ;; For simplicity this retrieves the cert and processes it locally
  (second
   (find field
         (mapcar (lambda (x)
                   (cl-ppcre:split "=" x))
                 (cl-ppcre:split "\\/"
                                 (execute-command (localhost)
                                                  "openssl"
                                                  (list "x509" :noout :subject
                                                               :in "/dev/stdin")
                                                  :output :first-line
                                                  :input (existing-content (part pair "cert")))))
         :key 'first :test 'equal)))

;; now we have to override the update plan because we don't want to check the file content
(defmethod requires-rebuild-p ((pair rsa-certificate-pair))
  ;; if the certificate is valid then it doesn't require rebuild, otherwise it does
  ;; It would be a Very Good Idea to allow some grace time to rebuild the certificate
  ;; !!! Need to do more verification, including checking the common name
  (with-temporary-resources
      ((cert (temporary-file (existing-content (part pair "cert"))))
       (ca (temporary-file (certificate-authority pair))))
    (not (and (cl-ppcre:scan ": OK$"
                             (or (ignore-errors
                                  (execute-command (localhost)
                                                   "openssl"
                                                   (list "verify" "-CAfile"
                                                         (full-path ca)
                                                         (full-path cert))
                                                   :output :first-line
                                                   :input (certificate-authority pair)))
                                 ""))
              (equal (common-name pair)
                     (get-existing-field pair "CN"))))))


;; Create the actual certificate in this method
(defmethod create ((pair rsa-certificate-pair))
  ;; we have to generate some file content
  (multiple-value-bind (key cert)
      (signed-host-key (certificate-authority pair)
                       (certificate-authority-key pair)
                       :common-name (common-name pair))
    (setf (slot-value (part pair "cert") 'content) cert
          (slot-value (part pair "key") 'content) key)))


(defmethod destroy-plan ((pair rsa-certificate-pair))
  (reduce #'append (mapcar #'destroy-plan (subcomponents pair))))
