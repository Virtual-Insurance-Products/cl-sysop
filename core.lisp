
(in-package :cl-sysop)

;; anything which is a component of something else
;; this will include lots of things
;; normally you won't set the parent directly - the initialize-instance will do it
(defclass component ()
  ((parent :initarg :parent :accessor parent)))


;; A system has components
(defclass system ()
  ())

;; This isn't really a system if it doesn't have subcomponents
(defmethod subcomponents ((s system)) nil)

;; we might want to do other things at adoption time
;; this will make it easy to inspect things
(defmethod adopt ((parent t) (c component))
  (setf (parent c) parent)
  c)

(defun adopter (system)
  (lambda (child)
    (adopt system child)))

(defmethod initialize-instance :after ((system system) &rest args)
  (declare (ignore args))
  (dolist (x (subcomponents system))
    (adopt system x)))



(defclass named ()
  ((name :initarg :name :accessor name)))

(defmethod print-object ((instance named) stream)
  (if (slot-boundp instance 'name)
      (print-unreadable-object (instance stream)
        (let* ((class (class-of instance))
               (class-name (class-name class)))
          (format stream "~A ~A" class-name (name instance))))
      (call-next-method)))


(defclass without (system component)
  ((subcomponents :initarg :subcomponents :reader subcomponents)))

(defun without (&rest components)
  (make-instance 'without :subcomponents components))

;; (localhost (without (make-instance 'brew-package :name "ledger")))


;; This simply ensures that create calls return the created component
(defmethod create :around ((x component))
  (call-next-method)
  x)
