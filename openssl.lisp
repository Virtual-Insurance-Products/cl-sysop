
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
                                           (org "VIP")
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
