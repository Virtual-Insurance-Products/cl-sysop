
(in-package :cl-sysop)

;; the following is the Hashicorp public key which I have retrieved
;; from that company's website AND from keybase. I checked that they
;; match. When I commit this into GIT the commit will be signed with
;; my GPG key thus providing a chain of trust.

;; if I could get some ED25519 keys generated which could be trusted
;; and stored on a secure machine then I would sign this with
;; that/those as well. 
(defparameter *hashicorp-gpg-public-key*
  "-----BEGIN PGP PUBLIC KEY BLOCK-----

mQENBFMORM0BCADBRyKO1MhCirazOSVwcfTr1xUxjPvfxD3hjUwHtjsOy/bT6p9f
W2mRPfwnq2JB5As+paL3UGDsSRDnK9KAxQb0NNF4+eVhr/EJ18s3wwXXDMjpIifq
fIm2WyH3G+aRLTLPIpscUNKDyxFOUbsmgXAmJ46Re1fn8uKxKRHbfa39aeuEYWFA
3drdL1WoUngvED7f+RnKBK2G6ZEpO+LDovQk19xGjiMTtPJrjMjZJ3QXqPvx5wca
KSZLr4lMTuoTI/ZXyZy5bD4tShiZz6KcyX27cD70q2iRcEZ0poLKHyEIDAi3TM5k
SwbbWBFd5RNPOR0qzrb/0p9ksKK48IIfH2FvABEBAAG0K0hhc2hpQ29ycCBTZWN1
cml0eSA8c2VjdXJpdHlAaGFzaGljb3JwLmNvbT6JAU4EEwEKADgWIQSRpuf4XQXG
VjC+8YlRhS2HNI/8TAUCXn0BIQIbAwULCQgHAgYVCgkICwIEFgIDAQIeAQIXgAAK
CRBRhS2HNI/8TJITCACT2Zu2l8Jo/YLQMs+iYsC3gn5qJE/qf60VWpOnP0LG24rj
k3j4ET5P2ow/o9lQNCM/fJrEB2CwhnlvbrLbNBbt2e35QVWvvxwFZwVcoBQXTXdT
+G2cKS2Snc0bhNF7jcPX1zau8gxLurxQBaRdoL38XQ41aKfdOjEico4ZxQYSrOoC
RbF6FODXj+ZL8CzJFa2Sd0rHAROHoF7WhKOvTrg1u8JvHrSgvLYGBHQZUV23cmXH
yvzITl5jFzORf9TUdSv8tnuAnNsOV4vOA6lj61Z3/0Vgor+ZByfiznonPHQtKYtY
kac1M/Dq2xZYiSf0tDFywgUDIF/IyS348wKmnDGjuQENBFMORM0BCADWj1GNOP4O
wJmJDjI2gmeok6fYQeUbI/+Hnv5Z/cAK80Tvft3noy1oedxaDdazvrLu7YlyQOWA
M1curbqJa6ozPAwc7T8XSwWxIuFfo9rStHQE3QUARxIdziQKTtlAbXI2mQU99c6x
vSueQ/gq3ICFRBwCmPAm+JCwZG+cDLJJ/g6wEilNATSFdakbMX4lHUB2X0qradNO
J66pdZWxTCxRLomPBWa5JEPanbosaJk0+n9+P6ImPiWpt8wiu0Qzfzo7loXiDxo/
0G8fSbjYsIF+skY+zhNbY1MenfIPctB9X5iyW291mWW7rhhZyuqqxN2xnmPPgFmi
QGd+8KVodadHABEBAAGJATwEGAECACYCGwwWIQSRpuf4XQXGVjC+8YlRhS2HNI/8
TAUCXn0BRAUJEvOKdwAKCRBRhS2HNI/8TEzUB/9pEHVwtTxL8+VRq559Q0tPOIOb
h3b+GroZRQGq/tcQDVbYOO6cyRMR9IohVJk0b9wnnUHoZpoA4H79UUfIB4sZngma
enL/9magP1uAHxPxEa5i/yYqR0MYfz4+PGdvqyj91NrkZm3WIpwzqW/KZp8YnD77
VzGVodT8xqAoHW+bHiza9Jmm9Rkf5/0i0JY7GXoJgk4QBG/Fcp0OR5NUWxN3PEM0
dpeiU4GI5wOz5RAIOvSv7u1h0ZxMnJG4B4MKniIAr4yD7WYYZh/VxEPeiS/E1CVx
qHV5VVCoEIoYVHIuFIyFu1lIcei53VD6V690rmn0bp4A5hs+kErhThvkok3c
=+mCN
-----END PGP PUBLIC KEY BLOCK-----")

;; This can probably be generalised to other hashicorp software
(defun hashicorp-releases (package)
  (cl-ppcre:all-matches-as-strings (format nil "/~A/\\d+\\.\\d+\\.\\d+/" package)
                                   (drakma:http-request (format nil "https://releases.hashicorp.com/~A/" package))))

;; latest consul release: (first (hashicorp-releases "consul"))
;; latest consul release: (first (hashicorp-releases "vault"))




(defun hashicorp-release-files (release)
  (cl-ppcre:all-matches-as-strings (format nil "~A[^\\\"]+" release)
                                   (drakma:http-request (format nil "https://releases.hashicorp.com~A"
                                                                release))))

;; (find "SHA256SUMS" (hashicorp-release-files (first (hashicorp-releases "consul"))) :test #'cl-ppcre:scan)

;; File is a regex
(defun hashicorp-release-file (release file)
  (unless (cl-ppcre:scan "^/" release)
      (setf release (first (hashicorp-releases release))))
  (format nil "https://releases.hashicorp.com~A"
          (find file (hashicorp-release-files release)
                :test #'cl-ppcre:scan)))
;; (hashicorp-release-file "consul" "\\.sig$")
;; (hashicorp-release-file "vault" "\\.sig$")
;; (hashicorp-release-file (first (hashicorp-releases "consul")) "\\.sig$") ; latest
;; (hashicorp-release-file (second (hashicorp-releases "consul")) "\\.sig$") ; previous

(defmethod hashicorp-platform-binary ((host darwin-host) release)
  (hashicorp-release-file release "darwin_amd64\\.zip"))

(defmethod hashicorp-platform-binary ((host solaris-host) release)
  (hashicorp-release-file release "solaris_amd64\\.zip"))

;; (hashicorp-platform-binary (localhost) "consul")

(defun get-hashicorp-release-file (release file)
  (drakma:http-request (hashicorp-release-file release file)))

;; sums, signature
;; (get-hashicorp-release-file "consul" "SHA256SUMS")
;; (get-hashicorp-release-file (first (hashicorp-releases "consul")) "\\.sig$")

;; now, how to verify the signature? 
;; I need to pick out the SHA sum that I need

;; In a way it's a shame that the sums are for the zipped files, because it means I won't be able to verify an existing installed binary. 
;; I guess I can just check that there is an installed binary of what I want. If not I'll download it. I can also check the version
;; I can just handle this in a specific consul/whatever binary class. 

;; I can save a file locally. It would be quite good if I could save it remotely to, but that would either require using a remote program like CURL or wget /or/ it would require piping the file content to cat.
;; That wouldn't be super fast compared to what should happen.

;; Maybe I should make a general verified download thing if I can work out how to generally do that.
;; OTOH, initially having verified hashicorp downloads would be good.
;; It means I would need GPG on the destination. 

;; The SmartOS GZ seems to have that built in already. It uses /root/.gnupg

;; This could be moved elsewhere to a general downloading thing
;; It will probably be needed a lot
(defun download (host url &key destination-directory check-certificate)
  (when (stringp destination-directory)
    (setf destination-directory
          (adopt host
                 (make-instance 'fs-directory
                                :full-path destination-directory))))
  
  (execute-command host
                   "wget"
                   ;; We won't bother with certs. It won't matter since we'll verify downloads in a much better way
                   `(,@ (unless check-certificate
                          `("--no-check-certificate"))
                     ,@ (when destination-directory
                          (list "-P" (full-path destination-directory)))
                     ,url))

  (let ((name (cl-ppcre:regex-replace ".*/" url "")))
    (if destination-directory
        (adopt destination-directory
               (make-instance 'fs-file :name name))
        (adopt host
               (make-instance 'fs-file
                              :name name
                              :full-path name)))))



(defmethod installed-hashicorp-package ((h unix-host) package)
  (ignore-errors (execute-command h package :v :output :first-line)))

;; (installed-hashicorp-package (localhost) "consul")
;; (installed-hashicorp-package (liganc) "consul")

;; I should be able to ask for the latest version. 
(defmethod installed-release-p ((h unix-host) package &optional (version (first (hashicorp-releases package))))
  (flet ((version (string)
           (cl-ppcre:regex-replace ".*(\\d\\.\\d\\.\\d).*"
                                   string
                                   "\\1")))
    (let ((installed (or (installed-hashicorp-package h package)
                         "<none>"))
          (available version))
      (values
       (equal (version installed)
              (version available))
       installed available))))


;; (installed-release-p (liganc) "consul")
;; (installed-release-p (localhost) "consul")
;; or ask for a specific version, but there's probably not much point
;; (installed-release-p (localhost) "consul" "/consul/1.8.0/") 
;; (installed-release-p (localhost) "vault")



;; A component

(defclass hashicorp-binary (named component)
  ((version :initform :latest :reader version)))

;; We aren't quite distinguishing between installed but not up to date...
(defmethod exists-p ((b hashicorp-binary))
  (installed-release-p (host b) (name b) (if (eql (version b) :latest)
                                             (first (hashicorp-releases (name b)))
                                             (version b))))

;; Just to make it explicit that we want to install 
(defmethod create-plan ((b hashicorp-binary))
  (flet ((bin (n)
           (update-plan (adopt (host b)
                               (make-instance 'installed-binary :name n)))))
    ;; we have to install the following binaries if they aren't there already
    ;; For smartOS hosts we should also require pkgin to have been setup
    ;; I have a script which does all that elsewhere to use
    ;; Indeed, installed-binary could, itself, trigger the installation of the package manager
    ;; in that way I could auto install brew onto a Mac host, which would be pretty handy.
    ;; Gradually this will turn into a very powerful thing for setting stuff up but will ONLY do the minimum needed
    ;; to get things running
    (append (bin "unzip")
            (bin "gpg")
            `((install ,b)))))


;; ??? Move to hosts.lisp and find a better way to do this
(defmethod binary-installation-directory ((h unix-host))
  "/opt/local/bin")

;; This is obviously a bit idiosyncratic
(defmethod binary-installation-directory ((h darwin-host))
  ;; with brew normal users have permission to install into here
  "/usr/local/bin")

;; If I can manage to figure this bit out I'll be doing pretty well...
(defmethod install ((b hashicorp-binary))
  (let ((host (host b)))
    (with-temporary-resources
        ((dir (temporary-directory nil host)))
      ;; maybe it would be good if this stuck the file into the directory object
      (flet ((fetch (what)
               (download host what :destination-directory dir)))
        (let* ((sums (fetch (hashicorp-release-file (name b) "SHA256SUMS")))
               (sig (fetch (hashicorp-release-file (name b) "\\.sig$")))
               (zip (fetch (hashicorp-platform-binary host (name b)))))

          ;; handle the key
          (execute-command host "gpg" (list "--import") :input *hashicorp-gpg-public-key*)
          
          ;; I'm going to intentionally bork the sigs to check gpg fail
          #+nil(create (adopt host
                         (make-instance 'fs-file
                                        :full-path (full-path sums)
                                        :content (concatenate 'string
                                                              (existing-content sums)
                                                              "some extra stuff"))))

          (execute-command host "gpg"
                           (list "--verify"
                                 (full-path sig)
                                 (full-path sums)))
          
          (let* ((sum (first (cl-ppcre:split "\\s+"
                                             (find (name zip)
                                                   (execute-command host
                                                                    "cat" (full-path sums)
                                                                    :output :lines)
                                                   :test #'cl-ppcre:scan))))
                 (sum-file
                   ;; Grab the sum we need
                   (create (adopt dir
                                  (make-instance 'fs-file
                                                 :name "sum"
                                                 :content (format nil
                                                                  "~A  ~A~%"
                                                                  sum (full-path zip)))))))
            ;; check it
            (execute-command host
                             "shasum"
                             (list :a "256" :c (full-path sum-file))))

          (ignore-errors (execute-command host
                                          "rm"
                                          (concatenate 'string
                                                       (binary-installation-directory host)
                                                       "/"
                                                       (name b))))
          
          ;; if that was ok then we proceed to unzip
          (execute-command host
                           "unzip"
                           (list
                            "-u" (full-path zip)
                            "-d" (binary-installation-directory host))))))))


;; For generating configs for hashicorp stuff...
;; !!! should I put brackets around it all?
(defun alist-to-hcl-string (alist)
  (with-output-to-string (stream)
    (loop for (key . value) in alist
          do (format stream "~(~A~) = ~A~%"
                     (cl-ppcre:regex-replace-all "-" (symbol-name key) "_")
                     (cond ((eql value t)
                            "true")
                           ((not value)
                            "false")
                           ((stringp value)
                            (format nil "~S" value))
                           ((listp value)
                            (format nil "{~%~A}"
                                    (alist-to-hcl-string value)))
                           (t value))
                     ))))
