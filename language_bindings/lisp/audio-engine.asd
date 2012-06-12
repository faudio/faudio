;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :asdf)

(defsystem :audio-engine
  :version "0.0.1"
  :description "The ScoreCleaner Audio Engine"
  :author "Hans Hoglund <hans.hoglund@doremir.com>"
  :maintainer "Hans Hoglund <hans.hoglund@doremir.com>"
  :depends-on (#+cffi :cffi)
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:module "backends"
     :components
     (#+lispworks (:file "lispworks")
      #+lispworks (:file "lispworks-gen")
      #+cffi      (:file "cffi")))
   (:file "base")
   (:file "device")
   (:file "scheduling")
   (:file "processor")
   (:file "stream")))
