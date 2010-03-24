;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; main.lisp --- GLUT Event Processing API.
;;;
;;; Copyright (c) 2006, Luis Oliveira <loliveira@common-lisp.net>
;;;   All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;  o Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;  o Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;  o Neither the name of the author nor the names of the contributors may
;;;    be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-glut)

(defcfun ("glutMainLoop" %glutMainLoop) :void)

#+darwin
(defcfun ("glutCheckLoop" check-loop) :void)

#-darwin
(defun main-loop ()
  (without-fp-traps
    (%glutMainLoop))
  (init))

#+(and darwin (or openmcl-native-threads sb-thread))
(defun interrupt-thread (thread function)
  #+ccl (ccl:process-interrupt thread function)
  #+sbcl (sb-thread:interrupt-thread thread function))

#+darwin
(let ((darwin-run-main-loop-p t))
  (defun main-loop ()
    (flet ((%loop ()
             (without-fp-traps
               (loop while darwin-run-main-loop-p do (check-loop)))
             (init)
             (setf darwin-run-main-loop-p t)))
      #+(or openmcl-native-threads sb-thread)
      (interrupt-thread *glut-thread* #'%loop)
      #-(or openmcl-native-threads sb-thread)
      (%loop)))
  (defun leave-main-loop ()
    (setf darwin-run-main-loop-p nil)))

#-darwin (defcfun ("glutMainLoopEvent" main-loop-event) :void)
#-darwin (defcfun ("glutLeaveMainLoop" leave-main-loop) :void)