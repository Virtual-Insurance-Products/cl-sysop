
(asdf:defsystem #:cl-sysop
  :description "System Operations"
  :author "VIP"
  :license "vip"
  :depends-on ("cl-ppcre" "anaphors" "diff" "vip-utils" "cl-json" "xmls" "drakma" "cybertiggyr-time")
  :serial t
  ;; would be good to put proper deps in
  :components ((:file "package")
               (:file "core")
               (:file "json")
               (:file "hosts")
               (:file "plan")
               (:file "packages")
               (:file "shell")
               (:file "filesystem")
               (:file "openssl")
               (:file "service")
               (:file "smf")
               (:file "vmadm")
               (:file "docker")
               (:file "installed-binary")
               (:file "hashicorp")
               (:file "consul")
               (:file "postgres-config")
               (:file "postgres")
               (:file "internal-zones")
               (:file "stunnel") ; used to do mTLS routing between servers
               (:file "sshd")))
