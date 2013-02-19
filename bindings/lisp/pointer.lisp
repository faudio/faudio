
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(in-package :audio-engine)

(defcfun (is-bool#      "doremir_is_bool") :boolean (a :pointer))
(defcfun (is-int8#      "doremir_is_int8") :boolean (a :pointer))
(defcfun (is-int16#     "doremir_is_int16") :boolean (a :pointer))
(defcfun (is-int32#     "doremir_is_int32") :boolean (a :pointer))
(defcfun (is-int64#     "doremir_is_int64") :boolean (a :pointer))
(defcfun (is-float#     "doremir_is_float") :boolean (a :pointer))
(defcfun (is-double#    "doremir_is_double") :boolean (a :pointer))
(defcfun (is-ref#       "doremir_is_ref") :boolean (a :pointer))

(defcfun (to-bool#      "doremir_to_bool") :boolean (a :pointer))
(defcfun (to-int8#      "doremir_to_int8") :int8 (a :pointer))
(defcfun (to-int16#     "doremir_to_int16") :int16 (a :pointer))
(defcfun (to-int32#     "doremir_to_int32") :int32 (a :pointer))
(defcfun (to-int64#     "doremir_to_int64") :int64 (a :pointer))
(defcfun (to-float#     "doremir_to_float") :float (a :pointer))
(defcfun (to-double#    "doremir_to_double") :double (a :pointer))

(defcfun (peek-bool#    "doremir_peek_bool") :boolean (a :pointer))
(defcfun (peek-int8#    "doremir_peek_int8") :int8 (a :pointer))
(defcfun (peek-int16#   "doremir_peek_int16") :int16 (a :pointer))
(defcfun (peek-int32#   "doremir_peek_int32") :int32 (a :pointer))
(defcfun (peek-int64#   "doremir_peek_int64") :int64 (a :pointer))
(defcfun (peek-float#   "doremir_peek_float") :float (a :pointer))
(defcfun (peek-double#  "doremir_peek_double") :double (a :pointer))

(defcfun (from-bool#    "doremir_from_bool") :pointer (a :boolean))
(defcfun (from-int8#    "doremir_from_int8") :pointer (a :int8))
(defcfun (from-int16#   "doremir_from_int16") :pointer (a :int16))
(defcfun (from-int32#   "doremir_from_int32") :pointer (a :int32))
(defcfun (from-int64#   "doremir_from_int64") :pointer (a :int64))
(defcfun (from-float#   "doremir_from_float") :pointer (a :float))
(defcfun (from-double#  "doremir_from_double") :pointer (a :double))

(defcfun (destroy#      "doremir_destroy") :void (a :pointer))

(defcfun (string-from-utf8# "doremir_string_from_utf8") :pointer (a :pointer))
(defcfun (string-to-utf8#   "doremir_string_to_utf8") :pointer (a :pointer))


(define-foreign-type ptr-type () () (:actual-type :pointer))
(define-parse-method ptr () (make-instance 'ptr-type))

(defmethod translate-to-foreign (x (type ptr-type))
  (etypecase x
    (cl:boolean         (from-bool# x))
    (cl:integer         (from-int32# x))
    (cl:ratio           (export-ratio# x))
    (cl:float           (from-float# x))
    (cl:double-float    (from-double# x))
    (cl:string          (string-from-utf8# (foreign-string-alloc x :encoding :utf-8)))
    (t                  (to-pointer x))))

; TODO leaks with boxed types
(defmethod translate-from-foreign (x (type ptr-type))
  (cond
    ((null-pointer-p x)  nil)
    ((is-bool# x)       (to-bool# x))
    ((is-int8# x)       (to-int8# x))
    ((is-int16# x)      (to-int16# x))
    ((is-int32# x)      (peek-int32# x))
    ((is-int64# x)      (peek-int64# x))
    ((is-float# x)      (peek-float# x))
    ((is-double# x)     (peek-double# x))
    (t                  x)))


; FIXME causes segmentation error with map-add
; (defmethod free-translated-object (x (type ptr-type) a) (declare (ignore a))
;   (destroy# x))

(defun to-pointer (x)
  "Convert a value to a pointer"
  (convert-to-foreign x (type-of x)))

(defun from-pointer (type x)
  "Convert a pointer to a value"
  (cond
    ((null x) nil)
    (t (convert-from-foreign x type))))




