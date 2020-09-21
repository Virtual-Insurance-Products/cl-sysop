
(in-package :cl-sysop)

(define-condition shell-error (error)
  ((error-code :initarg :code :reader error-code)
   (standard-error :initarg :standard-error :reader standard-error)))

(defmethod print-object ((x shell-error) (s stream))
  (format s "~A ~A"
          (error-code x)
          (if (slot-boundp x 'standard-error)
              (standard-error x) "")))

;; Things for interacting with shells
;; assumes local shell

;; If we need to invoke a subshell this escapes things reliably.
(defun escape-for-shell (string)
  (if (cl-ppcre:scan "^[a-zA-Z\\d\\-\\/\\.\\_]+$" string)
      string ; let's not escape the whatsit out of everything
      (with-output-to-string (stream)
        (write-char #\' stream)

        (loop for a across string
              do
                 (cond ((eq a #\')
                        (write-string "'\\''" stream))
                       (t (write-char a stream))))

        (write-char #\' stream))))

;; (escape-for-shell "Hi 'there")

;; I had something to escape for AppleScript as well if I need
;; that. Making login shells in new Terminal is sometimes useful for
;; example.

;; NOW, we need to be able to invoke commands using some kind of general interface
;; We will want to do a couple of things here:-
;; 1. pass input to write files using the shell
;; 2. grab stderr and/or show it
;; 3. check exit conditions



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Grabbed from Eve

;; this runs a shell command outputting character by character to stdio
;; I could probably have it output somewhere else easily enough
;; interrupting the lisp thread will terminate the process cleanly
;; I should make this throw an error if the exit status is non zero

;; NOTE that this is interpreted by the shell with all the implications which go along with that
;; I'm going to work it all like that though, because when we connect to remote systems we will be using shells on those anyway, so we might as well get used to it
(defun execute-with-shell (x &key input
                               (output *standard-output*)
                               (line-reader nil) ; pass a function which will be called on each line output
                               (echo-command t)
                               (error (make-string-output-stream)))
  ;; (declare (ignore error))
  (when echo-command
    (format *standard-output* "~%;;      ~A~%" x))
  (cond ((eq output :string)
         (with-output-to-string (stream)           
           (execute-with-shell x :input input :output stream :error error :echo-command nil)))

        ((eq output :lines)
         (cl-ppcre:split "\\n" (execute-with-shell x :input input :output :string :error error :echo-command nil)))

        ((eq output :first-line)
         (first (execute-with-shell x :input input :output :lines :error error :echo-command nil)))
        
        (t (let ((proc (ccl:run-program "bash"
                                        (list "-c" x)
                                        :wait nil
                                        :input input
                                        :error error
                                        :output :stream)))
             ;; !!! I need to grab the standard error too, but in another thread
             (unwind-protect (let ((stream (ccl:external-process-output-stream proc)))
                               (let ((v t)
                                     (this-line ""))
                                 (loop while v
                                       do
                                          (setf v (read-char stream nil))
                                          (when v
                                            (when line-reader
                                              (if (equal v #\Newline)
                                                  (progn (funcall line-reader this-line)
                                                         (setf this-line ""))
                                                  (setf this-line
                                                        (format nil "~A~A" this-line v))))
                                        
                                            (write-char v output)))))

               (multiple-value-bind (status code)
                   (ccl:external-process-status proc)
                 ;; wait a while in case we're too eager
                 (loop while (eq status :running)
                       do (sleep 0.01)
                          (multiple-value-bind (s c)
                              (ccl:external-process-status proc)
                            (setf status s code c)))
                 #+nil(unless (eq :exited status)
                        (break)
                        ;; !!! Sometimes this errors - not sure why
                        ;; I should catch the error and look at the signal instead of ignoring it
                        (ignore-errors (ccl:signal-external-process proc 2)))
                 ;; (error "~A - ~A" status code)
                 (when code
                   (unless (= 0 code)
                     (apply #'error
                            `(shell-error :code ,code
                                          ,@ (when (typep error 'ccl:string-output-stream)
                                               (list :standard-error (get-output-stream-string error))))
                            )
                     #+nil(error "Error: ~A ~A" code
                            (if (typep error 'ccl:string-output-stream)
                                (get-output-stream-string error) ""))))))))))

;; (>1 "ps aux")

;; SO, given a command and a list of arguments make a shell command
;; Before I had things like pipes (see below). Do I want to put this in again? Is there much point?
;; not sure. I might add them later - it would certainly work
(defun make-shell-command (command args)
  (unless (listp args)
    (setf args (list args)))
  
  (with-output-to-string (stream)
    (write-sequence command stream)
    (write-sequence " " stream)
    (loop for a in args
          do
             (write-sequence (cond ((stringp a)
                                    (escape-for-shell a))
                                   #+nil((eq a '\|)
                                         (setf after-cmd t)
                                         "|")
                                   #+nil((eq a '\;)
                                         (setf after-cmd t)
                                         ";")
                                   ((keywordp a)
                                    (concatenate 'string "-"
                                                 (string-downcase (symbol-name a))))
                                   ((symbolp a)
                                    (symbol-name a))
                                   ;; What's this for? I'll use it for redirection
                                   ((listp a)
                                    (if (second a)
                                        (format nil "~A~A" (first a) (escape-for-shell (second a)))
                                        (format nil "~A" (first a))))
                                   (t (format nil "~A" a)))
                             stream)
             (write-char #\space stream))))
;; (make-shell-command "ls" "/bin")
;; (make-shell-command "ls" '(:a "/bin"))
;; (make-shell-command "ls" '((> "foo.txt")))

