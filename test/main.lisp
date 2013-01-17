
(progn
  (asdf:load-system :doremir)
  (fli:register-module 
      :doremir-fw
      :real-name
        #+:macosx "/Users/hans/audio/build/Frameworks/DoReMIRAudio.framework/DoReMIRAudio"
      :connection-style :immediate)) 

(in-package :doremir)

; ---------------------------------------------------------------------------------------------------
; Special translators
; ---------------------------------------------------------------------------------------------------

; String 

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

; Override print by Doremir.String.Show
(defmethod print-object ((obj list) out)
  (format out "~a" (string-show (slot-value x 'list-ptr))))
(defmethod print-object ((obj buffer) out)
  (format out "~a" (string-show (slot-value x 'buffer-ptr))))
; etc

; Generate generic functions to wrap generic functions?
(less-than-equal (slot-value x 'buffer-ptr) (slot-value x 'buffer-ptr))
(less-than-equal (slot-value x 'list-ptr) (slot-value x 'list-ptr))

; TODO Safe versions of the type converters?

; ---------------------------------------------------------------------------------------------------

(setf x (buffer-create 1024))
(buffer-size x)
(buffer-peek x 1)
(buffer-poke x 1 10)
(cl:print x)
(buffer-destroy x)

(setf x (list-empty))
(setf x (list-dcons (from-int8 1) x))
(setf x (list-dcons (from-int8 118) x))
(setf x (list-tail x))
(list-length x)
(list-destroy x)
(cl:print (to-int8 (list-head x)))

(setf x (string-empty))
(setf x (string-single 104))
(string-destroy s)
(cl:print s)
(string-dappend (string-single 104) (string-dappend (string-single 97) (string-single 110)))
(string-append "hans" "höglund")
(string-length "högtalare")


(setf x (ratio-create 1 2))
(string-show x)
(destroy x)
(cl:print x)


