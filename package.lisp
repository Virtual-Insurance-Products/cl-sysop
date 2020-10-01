
(in-package :cl-user)

(defpackage :cl-sysop
  (:use :cl)
  (:export
   #:system
   #:component

   #:fs-directory
   #:fs-file
   ))

;; I'm just using this for json generation to avoid name conflicts...
(defpackage :json-property)


