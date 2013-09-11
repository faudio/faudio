
#|
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

#-sbcl (in-package :audio-engine)

(defvar *foreign-lib*)

; Load Lisp bindings
(asdf:load-system :fae)

; Load library and setup tests
(let ((framework-name "FAE")
      (framework-path (format nil "~a/audio/build/Frameworks/" (user-homedir-pathname)))
      (log-path       (format nil "~a/Library/Logs/FAE.log" (user-homedir-pathname))))
  (push framework-path cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library `(:framework ,framework-name)))
  (audio-engine::audioengine-set-log-file log-path)
  (audio-engine::plot-use-gnu)
  (audio-engine::audioengine-initialize))

; To unload, evaluate this
; (close-foreign-library *foreign-lib*)




