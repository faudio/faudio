
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

(defmethod print-object ((x buffer) out) (format out "~a" (string-show (slot-value x 'buffer-ptr))))
(defmethod print-object ((x midi) out) (format out "~a" (string-show (slot-value x 'midi-ptr))))
(defmethod print-object ((x list) out) (format out "~a" (string-show (slot-value x 'list-ptr))))
(defmethod print-object ((x pair) out) (format out "~a" (string-show (slot-value x 'pair-ptr))))
(defmethod print-object ((x set) out) (format out "~a" (string-show (slot-value x 'set-ptr))))
(defmethod print-object ((x map) out) (format out "~a" (string-show (slot-value x 'map-ptr))))
(defmethod print-object ((x ratio) out) (format out "~a" (string-show (slot-value x 'ratio-ptr))))
(defmethod print-object ((x atomic) out) (format out "~a" (string-show (slot-value x 'atomic-ptr))))
(defmethod print-object ((x priority-queue) out) (format out "~a" (string-show (slot-value x 'priority-queue-ptr))))
(defmethod print-object ((x processor) out) (format out "~a" (string-show (slot-value x 'processor-ptr))))
(defmethod print-object ((x scheduler) out) (format out "~a" (string-show (slot-value x 'scheduler-ptr))))
(defmethod print-object ((x signal) out) (format out "~a" (string-show (slot-value x 'signal-ptr))))
(defmethod print-object ((x thread) out) (format out "~a" (string-show (slot-value x 'thread-ptr))))
(defmethod print-object ((x time) out) (format out "~a" (string-show (slot-value x 'time-ptr))))
(defmethod print-object ((x type) out) (format out "~a" (string-show (slot-value x 'type-ptr))))


; etc

; ---------------------------------------------------------------------------------------------------

; Generate generic functions to wrap generic functions?

; ---------------------------------------------------------------------------------------------------

; TODO Safe versions of the type converters?   
