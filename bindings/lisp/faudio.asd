
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(defpackage #:faudio-asd
  (:use :cl :asdf))

(in-package :faudio-asd)

(defsystem :faudio
  :version "2.0.0"
  :description "Faudio"
  :author "hans.hoglund@doremir.com"
  :depends-on (:cffi)
  :serial t
  :components (
    (:file "package")
    (:file "pointer")
    (:file "fa")
    (:module "fa2"
      :pathname "fa"
      :components (
        (:file "std")
        (:file "string")
        (:file "list")
        (:file "pair")
        (:file "set")
        (:file "map")
        (:file "graph")
        (:file "priority-queue")
        (:file "ratio")
        (:file "dynamic")
        (:file "type")
        (:file "buffer")
        (:file "time")
        (:file "error")
        (:file "midi")
        (:module "midi2"
          :pathname "midi"
          :components (
            (:file "message")))
        (:file "audio")
        (:file "atomic")
        (:module "atomic2"
          :pathname "atomic"
          :components (
            (:file "queue")
            (:file "stack")
            (:file "ring-buffer")))
        (:file "signal")
        (:file "thread")
        (:file "fa")))
    (:file "special")
    (:file "utility")))

