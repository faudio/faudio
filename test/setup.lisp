(progn
  (asdf:load-system :audio-engine))

(progn
  (push "/Users/hans/audio/build/Frameworks/" cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library '(:framework "DoReMIRAudio"))))

; ---------------------------------------------------------------------------------------------------

; (in-package :audio-engine)
; 
; (defvar x nil)
; (defvar y nil)
; (defvar s nil)
; (defvar d nil)
; (defvar p nil)   
; 
