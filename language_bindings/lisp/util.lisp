;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;
          
(in-package :audio-engine)
                      
(defvar *backends* nil)      ;  pushed to when backends are loaded
(defvar +possible-backends+  ;  in reverse order of preference
  '(:lispworks 
    :cffi))

(defun default-backend-nil ()
  (reduce 
    (lambda (x y) (if (find y *backends*) y x)) 
    +possible-backends+ :initial-value nil))

(defun default-backend ()
  (let ((backend (default-backend-nil)))
    (if backend backend (error "No backend")))) 

(defun load-library ()
"Load the audio engine external library"
  (case (audio-engine::default-backend) 
    (:lispworks (lispworks-load-library))
    (:cffi      (cffi-load-library)))) 

(defun unload-library ()
"Unload the audio engine external library"
  (case (audio-engine::default-backend) 
    (:lispworks (lispworks-unload-library))
    (:cffi      (cffi-unload-library)))) 

(defmacro with-library (actions)
"This macro expands to a form that loads the audio engine external library, 
performs the given operations and the unloads the library."
  `(progn
    (load-library)
    ,actions
    (unload-library)))

(defmacro native-call (func type (&rest args) &key errors)
"This macro expands to a form that peforms a native call into the audio engine 
external library, using the backend returned by (default-backend) at expansion time."
  (case (audio-engine::default-backend) 
    (:lispworks (lispworks-call func type args errors))
    (:cffi      (cffi-call      func type args errors))))

