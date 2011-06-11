(defpackage pong-system
  (:use :common-lisp :asdf))
(in-package :pong-system)

(defvar *project-dir*
  (make-pathname :directory (pathname-directory *load-pathname*)))
(defvar *lib-dir*
  (merge-pathnames #P"lib/" *project-dir*))
(defvar *systems-dir*
  (merge-pathnames #P"systems/" *lib-dir*))

(defsystem "pong"
  :description "A simple pong game"
  :depends-on (:cl-cairo2 :cl-opengl :cl-glut :cl-glu :squirl) 
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "fw" :depends-on ("package"))
                                     (:file "pong" :depends-on ("package" "fw"))))))
