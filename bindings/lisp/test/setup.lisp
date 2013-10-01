
#|
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

#-sbcl (in-package :faudio)

(defvar *foreign-lib*)

; Deps
#|
  (pushnew "/Volumes/source/modus/cffi_0.10.7.1/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/babel/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/alexandria/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/trivial-features/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/faudio/" asdf:*central-registry* :test #'equal)
|#

; Load bindings (old ASDF)
(asdf:oos 'asdf:load-op :faudio)

; Force recompile
#|
  (asdf:operate 'asdf:load-op :faudio :force t)
|#
; (asdf:operate 'asdf:load-op :faudio :force t)


; Load library and setup tests
(let ((framework-name "Faudio")
      (framework-path  (format nil "~a/audio/build/Frameworks/" (user-homedir-pathname)))
      (framework-path2 (format nil "/Applications/LispWorks 6.1/LispWorks.app/Contents/Frameworks/"))
      (log-path        (format nil "~a/Library/Logs/Faudio.log" (user-homedir-pathname))))
  (push framework-path  cffi:*darwin-framework-directories*)
  (push framework-path2 cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library `(:framework ,framework-name)))
  (faudio::fa-set-log-file log-path)
  ;(faudio::plot-use-gnu)
  (faudio::fa-initialize)
  *foreign-lib*)

; Unload
#|
  (cffi:close-foreign-library *foreign-lib*)
|#




