
#|
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

#-sbcl (in-package :faudio)

(defvar *foreign-lib*)

; Load Lisp bindings
(asdf:load-system :faudio)
(asdf:operate 'asdf:load-op :faudio :force t)

; Load library and setup tests
(let ((framework-name "Faudio")
      (framework-path (format nil "~a/audio/build/Frameworks/" (user-homedir-pathname)))
      (log-path       (format nil "~a/Library/Logs/Fsound.log" (user-homedir-pathname))))
  (push framework-path cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library `(:framework ,framework-name)))
  (faudio::fa-set-log-file log-path)
  ;(faudio::plot-use-gnu)
  (faudio::fa-initialize)
)

; To unload, evaluate this
; (close-foreign-library *foreign-lib*)




