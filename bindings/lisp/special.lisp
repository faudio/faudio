
(in-package :doremir)

; ---------------------------------------------------------------------------------------------------

; Bootstrap imports using raw pointers (otherwise the translator will invoke itself forever)

(defcfun (string-from-utf8# "doremir_string_from_utf8") :pointer (a :pointer))
(defcfun (string-to-utf8#  "doremir_string_to_utf8") :pointer (a :pointer))
(defcfun (string-destroy# "doremir_string_destroy") :void (a :pointer))

(defmethod translate-to-foreign (x (type string-type))
  (string-from-utf8# (foreign-string-alloc x :encoding :utf-8))) ; TODO free temporary
(defmethod translate-from-foreign (x (type string-type))
  (foreign-string-to-lisp (string-to-utf8# x) :encoding :utf-8)) ; TODO free temporary
(defmethod free-translated-object (x (type string-type) a) (declare (ignore a))
  (string-destroy# x))

; ---------------------------------------------------------------------------------------------------

(defcfun (ratio-num# "doremir_ratio_num") :int32 (a :pointer))
(defcfun (ratio-denom# "doremir_ratio_denom") :int32 (a :pointer))
(defcfun (ratio-create# "doremir_ratio_create") :pointer (a :int32) (b :int32))
(defcfun (ratio-destroy# "doremir_ratio_destroy") :void (a :pointer))

(defun export-ratio# (x)
  (convert-to-foreign x 'ratio))

(defmethod translate-to-foreign (x (type ratio-type))
  (ratio-create# (numerator x) (denominator x))) 
(defmethod translate-from-foreign (x (type ratio-type))
  (/ (ratio-num# x) (ratio-denom# x))) 
(defmethod free-translated-object (x (type ratio-type) a) (declare (ignore a))
  (ratio-destroy# x))


; ---------------------------------------------------------------------------------------------------

(defun export-pair# (x)
  (pair-create (car x) (cdr x)))

(defun import-pair# (x)
  (cons (pair-fst x) (pair-snd x)))

(defun export-list# (x)
  ; nil -> []
  ; (cons x xs) -> (list-cons x xs)
  (cond
    ((not x)  (list-empty))
    (t        (list-cons (car x) (export-list# (cdr x))))))

(defun import-list# (x)
  ; [] -> nil
  ; (list-cons x xs) -> (cons x xs)
  (cond
    ((list-is-empty x)  nil)
    (t                  (cons (list-head x) (import-list# (list-tail x))))))

; ---------------------------------------------------------------------------------------------------

(defun export-type# (x)
  (cond
    ((eq x :i8)        (type-simple 0))
    ((eq x :i16)       (type-simple 1))
    ((eq x :i32)       (type-simple 2))
    ((eq x :i64)       (type-simple 3))
    ((eq x :f32)       (type-simple 4))
    ((eq x :f64)       (type-simple 5))
    ((eq x :ptr)       (type-simple 6))
    ((consp x)
      (cond 
        ((eq :frame  (car x))     (type-frame (make-type (cadr x))))
        ((eq :vector (car x))     (type-vector (make-type (cadr x)) (caddr x)))
        ((eq :pair   (car x))     (type-pair (make-type (cadr x)) (make-type (caddr x))))
        (t                        (type-pair (make-type (car x)) (make-type (cdr x))))))
    ((eq (type-of x) 'type) (slot-value x 'type-ptr))))

(defmethod translate-to-foreign (x (type type-type))
  (export-type# x)) 
; (defmethod translate-from-foreign (x (type type-type))
  ; x) 

; TODO add to translator (with inverse)
(defun make-type (x)
  (export-type# x))
  
; ---------------------------------------------------------------------------------------------------

; Override print by String.show
(defcfun (string-show# "doremir_string_show") string (a :pointer))

(defmethod print-object ((x buffer) out) (format out "~a" (string-show# (slot-value x 'buffer-ptr))))

(defmethod print-object ((x midi) out) (format out "~a" (string-show# (slot-value x 'midi-ptr))))
(defmethod print-object ((x list) out) (format out "~a" (string-show# (slot-value x 'list-ptr))))
(defmethod print-object ((x pair) out) (format out "~a" (string-show# (slot-value x 'pair-ptr))))
(defmethod print-object ((x set) out) (format out "~a" (string-show# (slot-value x 'set-ptr))))
(defmethod print-object ((x map) out) (format out "~a" (string-show# (slot-value x 'map-ptr))))
(defmethod print-object ((x ratio) out) (format out "~a" (string-show# (slot-value x 'ratio-ptr))))

(defmethod print-object ((x atomic) out) (format out "~a" (string-show# (slot-value x 'atomic-ptr))))
(defmethod print-object ((x atomic-queue) out) (format out "~a" (string-show# (slot-value x 'atomic-queue-ptr))))
(defmethod print-object ((x atomic-stack) out) (format out "~a" (string-show# (slot-value x 'atomic-stack-ptr))))
(defmethod print-object ((x atomic-ring-buffer) out) (format out "~a" (string-show# (slot-value x 'atomic-ring-buffer-ptr))))

(defmethod print-object ((x priority-queue) out) (format out "~a" (string-show# (slot-value x 'priority-queue-ptr))))
(defmethod print-object ((x processor) out) (format out "~a" (string-show# (slot-value x 'processor-ptr))))
(defmethod print-object ((x scheduler) out) (format out "~a" (string-show# (slot-value x 'scheduler-ptr))))
(defmethod print-object ((x signal) out) (format out "~a" (string-show# (slot-value x 'signal-ptr))))
(defmethod print-object ((x thread) out) (format out "~a" (string-show# (slot-value x 'thread-ptr))))

(defmethod print-object ((x time) out) (format out "~a" (string-show# (slot-value x 'time-ptr))))
(defmethod print-object ((x type) out) (format out "~a" (string-show# (slot-value x 'type-ptr))))


; etc

; ---------------------------------------------------------------------------------------------------

; Generate generic functions to wrap generic functions?

; ---------------------------------------------------------------------------------------------------

; TODO Safe versions of the type converters?   
