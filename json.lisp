
(in-package :cl-sysop)

(defclass json-object ()
  ())

;; !!! Pull this into a JSON serialisable or something
;; !!! Also, ip is actually deprecated
(defmethod json-spec ((x json-object))
  (loop for slot in (ccl:class-slots (class-of x))
        for name = (ccl:slot-definition-name slot)
        for value = (when (slot-boundp x name)
                      (slot-value x name))
        when (and (equal (package-name (symbol-package name)) "JSON-PROPERTY")
                  (slot-boundp x name))
          collect (cons name
                        (cond ((eq (ccl:slot-definition-type slot)
                                   'object-list)
                               (mapcar #'json-spec value))
                              ((typep (slot-value x name) 'json-object)
                               (json-spec (slot-value x name)))
                              (t (slot-value x name))))))

(defclass json-named (json-object named)
  ())

(defmethod json-spec ((x json-named))
  (append (when (slot-boundp x 'name)
            `((:name . ,(name x))))
          (call-next-method)))

