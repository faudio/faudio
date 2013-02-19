
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(in-package :audio-engine)

(defvar x nil)
(defvar y nil)
(defvar z nil)
(defvar s nil)
(defvar d nil)
(defvar p nil)

(defvar *foreign-lib*)

; Load Lisp bindings
(asdf:load-system :audio-engine)

; Load library and setup tests
(let ((framework-name "DoReMIRAudio")
      (framework-path (format nil "~a/audio/build/Frameworks/" (user-homedir-pathname)))
      (log-path       (format nil "~a/Library/Logs/DoReMIRAudio.log" (user-homedir-pathname))))
  (push framework-path cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library `(:framework ,framework-name)))
  (audioengine-set-log-file log-path)
  (plot-use-gnu)
  (audioengine-initialize))


#|
  ; Unload library
  (close-foreign-library *foreign-lib*)
|#

