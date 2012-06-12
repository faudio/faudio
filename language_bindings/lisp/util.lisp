;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;
          
(in-package :audio-engine)
                      
(defvar *backends* nil)                           ;  pushed to when backends are loaded
(defvar *possible-backends* '(:lispworks :cffi))  ;  in reverse order of preference

(defun default-backend ()
  (let ((backend (default-backend-nil)))
    (if backend backend (error "No backend")))) 

(defun default-backend-nil ()
  (reduce (lambda (x y) (if (find y *backends*) y x)) *possible-backends* :initial-value nil))

(defun load-library ()
  (case (audio-engine::default-backend) 
    (:lispworks (lispworks-load-library))
    (:cffi      (cffi-load-library)))) 

(defmacro native-call (func type (&rest args) &key errors)
  (case (audio-engine::default-backend) 
    (:lispworks (lispworks-call func type args errors))
    (:cffi      (cffi-call      func type args errors))))

