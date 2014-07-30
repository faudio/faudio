
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(defpackage #:faudio-asd
  (:use :cl :asdf))

(in-package :faudio-asd)

(defsystem :faudio
  :version "2.16.0"
  :description "Faudio"
  :author "support@doremir.com"
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
        (:file "list")
        (:file "pair")
        (:module "pair2"
          :pathname "pair"
          :components (
            (:file "left")))
        (:file "string")
        (:file "set")
        (:file "map")
        (:file "priority-queue")
        (:file "ratio")
        (:file "dynamic")
        (:file "buffer")
        (:file "time")
        (:file "clock")
        (:file "error")
        (:file "signal")
        (:file "action")
        (:file "midi")
        (:module "midi2"
          :pathname "midi"
          :components (
            (:file "session")
            (:file "device")
            (:file "stream")
            (:file "message")))
        (:file "audio")
        (:module "audio2"
          :pathname "audio"
          :components (
            (:file "session")
            (:file "device")
            (:file "stream")))
        (:file "atomic")
        (:module "atomic2"
          :pathname "atomic"
          :components (
            (:file "queue")
            (:file "stack")
            (:file "ring-buffer")))
        (:file "thread")
        (:file "io")
        (:file "fa")))
    (:file "special")
    (:file "utility")))

