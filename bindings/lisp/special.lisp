
(in-package :doremir)

; ---------------------------------------------------------------------------------------------------
; String 
; ---------------------------------------------------------------------------------------------------

; Must reimport these as (Ptr -> Ptr)
(defcfun (string-destroy# "doremir_string_destroy") :void (a :pointer))
(defcfun (string-from-utf8# "doremir_string_from_utf8") :pointer (a :pointer))
(defcfun (string-to-utf8#  "doremir_string_to_utf8") :pointer (a :pointer))

(defmethod translate-to-foreign (x (type string-type))
  (string-from-utf8# (foreign-string-alloc x :encoding :utf-8))) ; TODO free temporary
(defmethod translate-from-foreign (x (type string-type))
  (foreign-string-to-lisp (string-to-utf8# x) :encoding :utf-8))
(defmethod free-translated-object (x (type string-type) a) (declare (ignore a))
  (string-destroy# x))

; ---------------------------------------------------------------------------------------------------

; Override print by Show interface

(defmethod print-object ((x list) out) (format out "~a" (string-show (slot-value x 'list-ptr))))
(defmethod print-object ((x ratio) out) (format out "~a" (string-show (slot-value x 'ratio-ptr))))
(defmethod print-object ((x buffer) out) (format out "~a" (string-show (slot-value x 'buffer-ptr))))
; etc

; ---------------------------------------------------------------------------------------------------

; Generate generic functions to wrap generic functions?

; ---------------------------------------------------------------------------------------------------

; TODO Safe versions of the type converters?   
