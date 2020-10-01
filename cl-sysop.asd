
(asdf:defsystem #:cl-sysop
  :description "System Operations"
  :author "VIP"
  :license "vip"
  :depends-on ("cl-ppcre" "anaphors" "diff" "vip-utils" "cl-json" "xmls" "drakma")
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
               ))
