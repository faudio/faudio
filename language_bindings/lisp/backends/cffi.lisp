;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)

(push :cffi *backends*)

(defun cffi-load-library ()
  nil)

(defun cffi-unload-library ()
  nil)

(defun cffi-call (func type args errors)
  nil)

(defconstant +intern-to-package+ :audio-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





