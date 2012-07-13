;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(defpackage #:audio-engine-asd
  (:use :cl :asdf))

(in-package :audio-engine-asd)

(defsystem :audio-engine
  :version "1.5.0"
  :description "The ScoreCleaner Audio Engine"
  :author "Hans Hoglund <hans.hoglund@doremir.com>"
  :maintainer "Hans Hoglund <hans.hoglund@doremir.com>"
  :depends-on (#+cffi :cffi)
  :serial t
  :components
  ((:file "package")
   (:file "misc")
   (:file "util")
   (:module "backends"
     :components
     (#+lispworks (:file "lispworks")
      #+lispworks (:file "lispworks-gen")
      #+cffi      (:file "cffi")))
   (:file "base")
   (:file "device")
   (:file "stream")
   (:file "scheduling")
   (:file "processor")))
