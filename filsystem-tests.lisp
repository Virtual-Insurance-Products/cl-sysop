
(in-package :cl-sysop)


;; this gets a file with a full path which is easy to work with
;; I can then simply make methods to read file content, get sha1 of file etc
;; I guess I'll need ironclad to sha1 the content to check for a match
(make-instance 'fs-directory
               :full-path "/Users/david"
               :content (list
                         (make-instance 'fs-file
                                        :name "test.txt"
                                        :content "this is a test file")))


;; the full path will be passed down to the file object
;; we don't really *need* to statically do that - we could just pull it when we need it via the full-path method
;; the advantage of doing it this way, though, is that we can use the CL inspector and see the value
;; maybe there should be a better solution to that, but it's simple enough and works
(defparameter *dir-test*
  (make-instance 'fs-directory
                 :full-path "/Users/david"
                 :content (list
                           (make-instance 'fs-directory
                                          :name "config"
                                          :content
                                          (list (make-instance 'fs-file
                                                               :name "test.txt"
                                                               :content "this is a test file"))))))


(host *dir-test*) ; doesn't work - unhosted directory

(defparameter *hosted-dir*
  (localhost *dir-test*))

;; now we can do this
(host (first (subcomponents *hosted-dir*)))

;; SO, given that a directory can get a host we can ask a directory, or a file, to do something...

(exists-p (first (subcomponents *hosted-dir*))) ; this directory exists
(exists-p (first (subcomponents (first (subcomponents *hosted-dir*))))) ; this does not

(update-plan *hosted-dir*)
;; - ok, that now plans properly.
;; it doesn't create the existing directory but does create the non-existent one AND the file in it.

;; (exists-p *hosted-dir*)

;; then I need to check file content etc to decide whether to update file content and permissions
;; then I need to implement a way of executing the plan. 
;; this is coming together
;; I wonder if my simple model will fail with anything. 

;; ASAP I would like to get it to the stage where I can provision
;; zones with it and then get stuff running in them. If I can set up a
;; zone doing something like serving web traffic with a simple
;; declarative thing like this, and then update it to change things
;; that will be Very Cool.

;; Then I'll need to think of a way to make build.sh scripts to invoke
;; this stuff and execute the update plan.

;; It would also be good to have a protocol to start the planning run
;; by clearing what we know about the state of the system so that when
;; we do things like checking to see if a package is installed, we can
;; get the list of installed packages once and then keep it in the
;; host object. Or something like that.

(loop for (op . args) in (update-plan *hosted-dir*)
      do (apply op args))

(execute-command (localhost) "rm" '(:r "/Users/david/config"))

;; ok, so far so good - we can describe a set of FS objects on a host. That's actually rather useful. It means I could drop in config files, including to remote hosts. I haven't done command execution in zones yet, so I should add that. 
;; Now I need to start describing zones and hosts and also to implement permissions, groups and owners

;; also utilities for making certs and stuff

; oh
