;;;-*- Mode: lisp -*-

;;
;;  DoReMIR Audio Engine
;;  Copyright (c) DoReMIR Music Research 2012-2013
;;  All rights reserved.
;;

(defpackage #:audio-engine-asd
  (:use :cl :asdf))

(in-package :audio-engine-asd)

(defsystem :audio-engine
  :version "2.0.0"
  :description "The DoReMIR Audio Engine"
  :author "hans.hoglund@doremir.com"
  :depends-on (:cffi)
  :serial t
  :components (
    (:file "package")
    (:file "ptr")
    (:file "doremir")
    (:module "doremir2"
      :pathname "doremir"
      :components (
        (:file "std")
        (:file "buffer")
        (:file "string")
        (:file "error")
        (:file "atomic")
        (:module "atomic2"
          :pathname "atomic"
          :components (
            (:file "queue")
            (:file "stack")
            (:file "ring-buffer")))
        (:file "pair")
        (:file "plot")
        (:file "list")
        (:file "set")
        (:file "map")
        (:file "priority-queue")
        (:file "ratio")
        (:file "time")
        (:file "message")
        (:file "midi")
        (:file "type")
        (:file "processor")
        (:file "signal")
        (:file "thread")
        (:module "thread2"
          :pathname "thread"
          :components (
            (:file "future")
            (:file "improving")))
        (:file "scheduler")
        (:file "device")
        (:module "device2"
          :pathname "device"
          :components (
            (:file "audio")
            (:file "midi")
            (:file "file")
            (:file "buffer")))
        (:file "audio-engine")))
    (:file "special")))

