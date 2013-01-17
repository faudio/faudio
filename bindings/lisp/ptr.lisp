
(in-package :doremir)

(defcfun (is-bool# "doremir_is_bool") :boolean (a :pointer))
(defcfun (is-int8# "doremir_is_int8") :boolean (a :pointer))
(defcfun (is-int16# "doremir_is_int16") :boolean (a :pointer))
(defcfun (is-int32# "doremir_is_int32") :boolean (a :pointer))
(defcfun (is-int64# "doremir_is_int64") :boolean (a :pointer))
(defcfun (is-float# "doremir_is_float") :boolean (a :pointer))
(defcfun (is-double# "doremir_is_double") :boolean (a :pointer))
(defcfun (is-ref# "doremir_is_ref") :boolean (a :pointer))

(defcfun (to-bool# "doremir_to_bool") :boolean (a :pointer))
(defcfun (to-int8# "doremir_to_int8") :int8 (a :pointer))
(defcfun (to-int16# "doremir_to_int16") :int16 (a :pointer))
(defcfun (to-int32# "doremir_to_int32") :int32 (a :pointer))
(defcfun (to-int64# "doremir_to_int64") :int64 (a :pointer))
(defcfun (to-float# "doremir_to_float") :float (a :pointer))
(defcfun (to-double# "doremir_to_double") :double (a :pointer))

(defcfun (from-bool# "doremir_from_bool") :pointer (a :boolean))
(defcfun (from-int8# "doremir_from_int8") :pointer (a :int8))
(defcfun (from-int16# "doremir_from_int16") :pointer (a :int16))
(defcfun (from-int32# "doremir_from_int32") :pointer (a :int32))
(defcfun (from-int64# "doremir_from_int64") :pointer (a :int64))
(defcfun (from-float# "doremir_from_float") :pointer (a :float))
(defcfun (from-double# "doremir_from_double") :pointer (a :double))
(defcfun (string-from-utf8# "doremir_string_from_utf8") :pointer (a :pointer))
(defcfun (string-to-utf8#  "doremir_string_to_utf8") :pointer (a :pointer))

(define-foreign-type ptr-type () () (:actual-type :pointer))
(define-parse-method ptr () (make-instance 'ptr-type))
; (defclass ptr () ((ptr-ptr :initarg :ptr-ptr)))

(defmethod translate-to-foreign (x (type ptr-type)) 
  
  ; (slot-value x 'ptr-ptr)
  (etypecase x
      (cl:boolean      (from-bool# x))
      (cl:float        (from-float# x))
      (cl:double-float (from-double# x))
      (cl:integer      (from-int16# x))
      (cl:string       (string-from-utf8# (foreign-string-alloc x :encoding :utf-8)))
      ; TODO 32 and 64-bit ints
      ; TODO ratio/rational (?)
      ; TODO list
      ; TODO cons                   
      ; TODO ptr (if already explicitly wrapped?)
      ; TODO other opaque (i.e. pair)
  )) 
(defmethod translate-from-foreign (x (type ptr-type)) 
  
  (cond
    ((is-bool# x)       (to-bool# x))
    ((is-int8# x)       (to-int8# x))
    ((is-int16# x)      (to-int16# x))
    ((is-int32# x)      (to-int32# x))
    ((is-int64# x)      (to-int64# x))
    ((is-float# x)      (to-float# x))
    ((is-double# x)     (to-double# x))
    ; TODO ratio, list, pair
    ; FIXME is nil always returned if none match?
    )  
  ; (make-instance 'ptr :ptr-ptr x)
  )

