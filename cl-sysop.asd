
(asdf:defsystem #:cl-sysop
  :description "System Operations"
  :author "VIP"
  :license "vip"
  :depends-on ("cl-ppcre" "anaphors" "diff" "vip-utils")
  :serial t
  ;; would be good to put proper deps in
  :components ((:file "package")
               (:file "core")
               (:file "plan")
               (:file "packages")
               (:file "hosts")
               (:file "shell")
               (:file "filesystem")
               (:file "vmadm")
               ))
