(defpackage pong-system
  (:use :common-lisp :asdf))
(in-package :pong-system)

(defvar *project-dir*
  (make-pathname :directory (pathname-directory *load-pathname*)))
(defvar *lib-dir*
  (merge-pathnames #P"lib/" *project-dir*))
(defvar *systems-dir*
  (merge-pathnames #P"systems/" *lib-dir*))

(pushnew *systems-dir* *central-registry*)
(oos 'load-op :asdf-install)

(defsystem "pong"
  :description "A simple pong game"
  :depends-on (:cl-cairo2 :cl-opengl :cl-glut :cl-glu :squirl) 
  :pathname *project-dir*
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "fw" :depends-on ("package"))
                                     (:file "pong" :depends-on ("package" "fw"))))))

(defun load-installing-dependencies ()
  (let ((locations asdf-install:*locations*))
    (setf asdf-install:*locations* 
      (list (list *lib-dir* *systems-dir* "Install to project library directory")))
    (unwind-protect
         (handler-bind ((missing-dependency (lambda (c) 
                                              (asdf-install:install (asdf::missing-requires c))
                                              (load-installing-dependencies))))
           (oos 'load-op :pong))
      (setf asdf-install:*locations* locations))))
       
