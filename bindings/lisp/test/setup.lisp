
#|
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

#-sbcl (in-package :faudio)

(defvar *foreign-lib*)

; Deps

#+cocoa
(progn
  (pushnew "/Volumes/source/modus/cffi_0.10.7.1/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/babel/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/alexandria/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/trivial-features/" asdf:*central-registry* :test #'equal)
  (pushnew "/Volumes/source/modus/faudio/" asdf:*central-registry* :test #'equal)
  )
#+win32
(progn
  (pushnew "C:\\Users\\Doremir\\Modus\\cffi_0.10.7.1\\" asdf:*central-registry* :test #'equal)
  (pushnew "C:\\Users\\Doremir\\Modus\\babel\\" asdf:*central-registry* :test #'equal)
  (pushnew "C:\\Users\\Doremir\\Modus\\alexandria\\" asdf:*central-registry* :test #'equal)
  (pushnew "C:\\Users\\Doremir\\Modus\\trivial-features\\" asdf:*central-registry* :test #'equal)
  (pushnew "C:\\Users\\Doremir\\Modus\\faudio\\" asdf:*central-registry* :test #'equal)
  )


; Load bindings (old ASDF)
;(setf asdf:*central-registry* (cl:list *default-pathname-defaults*))
;(cl:print asdf:*central-registry*)
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
  #+cocoa (setf *foreign-lib* (cffi:load-foreign-library `(:framework ,framework-name)))
  #+win32 (setf *foreign-lib* (cffi:load-foreign-library "C:\\Program Files (x86)\\LispWorks\\libfaudio.dll"))
  (faudio::fa-set-log-file log-path)
  ;(faudio::plot-use-gnu)
  (faudio::fa-initialize)
  *foreign-lib*)

; Unload
#|
  (cffi:close-foreign-library *foreign-lib*)
|#




