
(require "foreign-parser") 

(let*
  ((out *standard-output*))
  (format out "This is a little program!~%") 
  )

; (setf dir "/Users/hans/Documents/Kod/doremir/modus/")
; (setf input (concatenate 'string dir "audio/include/sclaudio.h"))
; (setf output (concatenate 'string dir "audio-fli.lisp"))
; (setf *preprocessor-include-path* (concatenate 'string dir "audio/include"))
; (setf package :audio)
; (setf prep "-DSCL_UNICODE")
; 
; (foreign-parser:process-foreign-file input 
;      :dff output 
;      :case-sensitive nil 
;      :package package
;      :preprocessor-options prep)

