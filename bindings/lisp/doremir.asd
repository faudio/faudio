;;;-*- Mode: lisp -*-

;;
;;  DoReMIR Audio Engine
;;  Copyright (c) DoReMIR Music Research 2012-2013
;;  All rights reserved.
;;

(defpackage #:doremir-asd
  (:use :cl :asdf))

(in-package :doremir-asd)

(defsystem :doremir
  :version "1.5.0"
  :description "The DoReMIR Audio Engine"
  :author "hans.hoglund@doremir.com"
  :depends-on (#+cffi :cffi)
  :serial t
  :components (
      (:module "doremir"
       :components (          
          (:file "atomic")
          (:file "audio-engine")
          (:file "buffer")
          (:file "list")
          (:file "map")
          (:file "message")
          (:file "midi")
          (:file "pair")
          (:file "priority-queue")
          (:file "processor")
          (:file "ratio")
          (:file "scheduler")
          (:file "set")
          (:file "signal")
          (:file "std")
          (:file "string")
          (:file "thread")
          (:file "time")
          (:file "type")
          ))
      (:file "doremir")
   ))

