
(in-package :cl-sysop)

;; requires-rebuild-p

(defgeneric component-exists-in-system-p (component system))

;; if it doesn't exist then we must create it
;; this is a general case, but won't always be the best way to determine this
(defmethod exists-p ((x component))
  (component-exists-in-system-p x (parent x)))

;; for a system we'll just assume it exists unless it has a method to determine otherwise
;; that's because in general a system is made of components and really we want to know if THEY exist
(defmethod exists-p ((x system))
  t)

;; define a way to create the component. Again, a general case
(defmethod create ((x component))
  (create-component-in-system x (parent x)))

;; #'create-component-in-system
(defgeneric create-component-in-system (component system))


;; #'update-plan
;; generally for any kind of thing THERE IS NO PLAN to update it
(defmethod update-plan ((x t) &optional without)
  (declare (ignore without))
  nil)

;; for a system we make a plan for all the components
;; in general for a system there isn't anything else we can do - we can't create the system unless it's a component of another system
;; how am I going to sequence the update plan? This next method isn't going to tend to contain anything
;; UNLESS you we put system in AFTER other things that we inherit which handle dependencies
;; that could work.
(defmethod update-plan ((system system) &optional without)
  (append (call-next-method)
          (reduce #'append
                  (mapcar (lambda (c)
                            (update-plan c without))
                          (subcomponents system)))))

;; this recurses down passing without
(defmethod update-plan ((system without) &optional without)
  (declare (ignore without))
  (reduce #'append
          (mapcar (lambda (component)
                    (update-plan component t))
                  (subcomponents system))))

;; Let's say that, in general, components don't need to be rebuilt if they exist
;; that stops us from destroying things willy nilly
(defmethod requires-rebuild-p ((x component)) nil)

;; the default CREATE plan for a component is...
(defmethod create-plan ((x component)) `((create ,x)))

(defmethod create-plan ((x system))
  (append (call-next-method)
          (reduce #'append
                  (mapcar #'create-plan
                          (subcomponents x)))))

;; This is quite vague
;; the plan ought to be more detailed than this
(defmethod update-plan :around ((x component) &optional without)
  (if without
      (when (exists-p x)
        (append (destroy-plan x)
                (create-plan x)
                (call-next-method)))
      (if (exists-p x)
          (if (requires-rebuild-p x)
              ;; FIXME - destroy subcomponents first if required, but it might not be
              (append (destroy-plan x)
                      (create-plan x))
              ;; otherwise we make the create plan
              (call-next-method))
          (create-plan x))))

(defmethod destroy-plan ((c component))
  `((destroy ,c)))

;; If it doesn't exist there isn't anything to do
(defmethod destroy-plan :around ((c component))
  (if (exists-p c)
      (call-next-method)
      nil))

(defmethod destroy-plan ((s system))
  (append (reduce #'append
                  (mapcar #'destroy-plan
                          (subcomponents s)))
          (call-next-method)))

;; if there is more that needs doing then obviously we have to define a new method for the particular system
(defmethod create ((s system))
  ;; !!! I don't need to create the subcomponents here - the create-plan unfolds all of this
  )

;; !!! Maybe I could make a component called (not) so that I can just negate any thing
;; then if it is installed we destroy it and vice versa
;; that will remove a lot of duplication



;; TODO - plan execution

(define-condition updates-required ()
  ((plan :initarg :plan :reader plan)))

(defmethod print-object ((c updates-required) (s stream))
  (format s "The following changes are required:-~%~%")
  (dolist (step (plan c))
    (format s "~S~%" step))
  (format s "~%--- END OF PLAN ---~%"))

(defun execute-plan (plan)
  (let ((steps (length plan)))
    (loop for (op . args) in plan
          for step from 1
          do (format t "~%********************************************************************************~%STEP ~A/~A :: ~A~%" step steps (cons op args))
             (if (eq op 'setf)
                 (funcall (fdefinition `(setf ,(first (first args))))
                          (second args) (second (first args)))
                 (apply op args)))))

;; this is intended for interactive use
;; for batch use do the other one
(defun update (system/component)
  (let ((plan (update-plan system/component)))
    (restart-case (when plan
                    (error 'updates-required :plan plan))
      (apply-changes ()
        :report "Execute the plan to apply changes"
        (execute-plan plan)))))

;; then we can use the above and just provide something to invoke the condition handler

(when nil
  (update (localhost (make-instance 'fs-file :full-path "/Users/david/blah.txt"
                                             :content "A humble text file")
                     (make-instance 'fs-file :full-path "/Users/david/blah-blah.txt"
                                             :content "A humble text file"))))

;; We can also define a system which DOES NOT have these things
(when nil
  (update (localhost (without (make-instance 'fs-file :full-path "/Users/david/blah.txt"
                                                      :content "A humble text file")
                              (make-instance 'fs-file :full-path "/Users/david/blah-blah.txt"
                                                      :content "A humble text file")))))


(defmacro with-temporary-resources (bindings &body forms)
  `(let ,bindings
     (let ((value nil))
       (unwind-protect
            (setf value
                  (progn
                    ,@forms))
         ,@ (loop for (var) in bindings
                  collect `(destroy ,var)))
       value)))

