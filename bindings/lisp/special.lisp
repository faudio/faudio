
(in-package :audio-engine)

; ---------------------------------------------------------------------------------------------------

(defcfun (string-from-utf8# "doremir_string_from_utf8") :pointer (a :pointer))
(defcfun (string-to-utf8#  "doremir_string_to_utf8") :pointer (a :pointer))
(defcfun (string-destroy# "doremir_string_destroy") :void (a :pointer))

(defmethod translate-to-foreign (x (type string-type))
  (string-from-utf8# 
    (foreign-string-alloc x :encoding :utf-8)))

(defmethod translate-from-foreign (x (type string-type))
  (foreign-string-to-lisp 
    (string-to-utf8# x) :encoding :utf-8))

(defmethod free-translated-object (x (type string-type) a) 
  (declare (ignore a))
  (string-destroy# x))

; ---------------------------------------------------------------------------------------------------

(defcfun (ratio-create# "doremir_ratio_create") :pointer (a :int32) (b :int32))
(defcfun (ratio-destroy# "doremir_ratio_destroy") :void (a :pointer))
(defcfun (ratio-num# "doremir_ratio_num") :int32 (a :pointer))
(defcfun (ratio-denom# "doremir_ratio_denom") :int32 (a :pointer))

(defmethod translate-to-foreign (x (type ratio-type))
  (ratio-create# (numerator x) (denominator x)))

(defmethod translate-from-foreign (x (type ratio-type))
  (/ (ratio-num# x) (ratio-denom# x)))

(defmethod free-translated-object (x (type ratio-type) a) 
  (declare (ignore a))
  (ratio-destroy# x))


; ---------------------------------------------------------------------------------------------------

(defcfun (pair-create# "doremir_pair_create") :pointer (a ptr) (b ptr))
(defcfun (pair-destroy# "doremir_pair_destroy") :void (a :pointer))
(defcfun (pair-fst# "doremir_pair_fst") ptr (a :pointer))
(defcfun (pair-snd# "doremir_pair_snd") ptr (a :pointer))

(defun export-pair# (x)
  (pair-create# (car x) (cdr x)))

(defun import-pair# (x)
  (cons (pair-fst# x) (pair-snd# x)))

; TODO auto-import?

(defun export-pair (x)
  (pair-create (car x) (cdr x)))

(defun import-pair (x)
  (cons (pair-fst x) (pair-snd x)))


; ---------------------------------------------------------------------------------------------------

(defcfun (list-empty# "doremir_list_empty") :pointer)
(defcfun (list-cons#  "doremir_list_cons")  :pointer (a ptr) (b :pointer))
(defcfun (list-dcons# "doremir_list_dcons") :pointer (a ptr) (b :pointer))
(defcfun (list-head#  "doremir_list_head") ptr (a :pointer))
(defcfun (list-tail#  "doremir_list_tail") :pointer (a :pointer))

(defun export-list# (x)
  (cond                
    ((null x)               (list-empty#)) 
    ((consp x)              (list-cons# (car x) (export-list# (cdr x))))
    ((eq (type-of x) 'list) (slot-value x 'list-ptr))))

(defun import-list# (x)
  (cond
    ((list-is-empty x)  nil)
    (t                  (cons (list-head x) (import-list# (list-tail x))))))

(defmethod translate-to-foreign (x (type list-type))
  (export-list# x))
(defmethod translate-from-foreign (x (type list-type)) 
    (make-instance 'list :list-ptr x))

(defun export-list (x)
  (cond                
    ((null x)               (list-empty)) 
    ((consp x)              (list-cons (car x) (export-list (cdr x))))
    ((eq (type-of x) 'list) (slot-value x 'list-ptr))))

(defun import-list (x)
  (cond
    ((list-is-empty x)  nil)
    (t                  (cons (list-head x) (import-list (list-tail x))))))


; ---------------------------------------------------------------------------------------------------

(defun export-type# (x)
  (cond
    ((eq x :unit)      (type-simple 0))
    ((eq x :i8)        (type-simple 1))
    ((eq x :i16)       (type-simple 2))
    ((eq x :i32)       (type-simple 3))
    ((eq x :i64)       (type-simple 4))
    ((eq x :f32)       (type-simple 5))
    ((eq x :f64)       (type-simple 6))
    ((eq x :ptr)       (type-simple 7))
    ((eq x nil)        (type-simple 0))
    ((consp x)
      (cond
        ((eq :frame  (car x))     (type-frame (to-type (cadr x))))
        ((eq :vector (car x))     (type-vector (to-type (cadr x)) (caddr x)))
        ((eq :pair   (car x))     (type-pair (to-type (cadr x)) (to-type (caddr x))))
        (t                        (type-pair (to-type (car x)) (to-type (cdr x))))))
    ((eq (type-of x) 'type) (slot-value x 'type-ptr))
    (t                      x)))

(defmethod translate-to-foreign (x (type type-type))
  (export-type# (export-type# x)))
(defmethod translate-from-foreign (x (type type-type)) 
    (make-instance 'type :type-ptr x))

(defun to-type (x)
  (export-type# x)) ; FIXME should not do slot-value??

; ---------------------------------------------------------------------------------------------------

(defun to-error (x)
  (from-pointer 'error (to-pointer x)))

; ---------------------------------------------------------------------------------------------------

(defun to-sender (x) (setf s (from-pointer 'message-sender (to-pointer x))))
(defun to-receiver (x) (setf r (from-pointer 'message-receiver (to-pointer x))))

; ---------------------------------------------------------------------------------------------------

(defun to-processor (x) (setf s (from-pointer 'processor (to-pointer x))))

; ---------------------------------------------------------------------------------------------------

(defun time (&key days 
                  hours 
                  minutes 
                  seconds 
                  milliseconds 
                  nanoseconds)
  (let* ((zero-time         (time-create 0 0 0 0))
         (days-time         (if days    (time-create days 0 0 0) nil))
         (hours-time        (if hours   (time-create 0 hours 0 0) nil))
         (minutes-time      (if minutes (time-create 0 0 minutes 0) nil))
         (seconds-time      (if seconds (time-create 0 0 0 (rational seconds)) nil))
         (milliseconds-time (if milliseconds (time-create 0 0 0 (/ (rational milliseconds) 1000)) nil))
         (nanoseconds-time  (if nanoseconds (time-create 0 0 0 (/ (rational nanoseconds) 1000000)) nil))
         (time-exprs  (remove nil (cl:list days-time hours-time minutes-time 
                                           seconds-time milliseconds-time nanoseconds-time))))
    (reduce (lambda (x y) 
      (from-pointer 'time (add x y))) time-exprs 
      :initial-value zero-time)))

(defun hours (x) (time :hours x))
(defun minutes (x) (time :minutes x))
(defun seconds (x) (time :seconds x))
(defun milliseconds (x) (time :milliseconds x))

; ---------------------------------------------------------------------------------------------------

(defvar *funcs#* (make-hash-table))

(defun func-to-int# (f)
   (let ((n (hash-table-count *funcs#*)))
     (setf (gethash n *funcs#*) f)
     n))

(defun int-to-func# (n)
  (let ((f (gethash n *funcs#*)))
    (if f f (cl:error "Uknown func in INT-TO-FUNC"))))

(defcallback funcall1# ptr ((f ptr) (x ptr))
  (funcall (int-to-func# f) x))

(defcallback predcall1# :boolean ((f ptr) (x ptr))
  (funcall (int-to-func# f) x))


(defun list-map* (f xs)   
  (list-map (callback funcall1#) (func-to-int# f) xs)) 

(defun list-join-map* (f xs)   
  (list-join-map (callback funcall1#) (func-to-int# f) xs)) 

(defun list-filter* (f xs)
  (list-filter (callback predcall1#) (func-to-int# f) xs))

(defun list-find* (f xs)
  (list-find (callback predcall1#) (func-to-int# f) xs))

(defun list-find-index* (f xs)
  (list-find-index (callback predcall1#) (func-to-int# f) xs))

; ---------------------------------------------------------------------------------------------------

(defun event-map* (f xs)   
  (event-map (callback funcall1#) (func-to-int# f) xs)) 

(defun event-filter* (f xs)
  (event-filter (callback predcall1#) (func-to-int# f) xs))
  

; ---------------------------------------------------------------------------------------------------

(defun string-map* (f xs)   
  (string-map (callback funcall1#) (func-to-int# f) xs)) 

(defun string-join-map* (f xs)   
  (string-join-map (callback funcall1#) (func-to-int# f) xs)) 

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

(defmethod print-object ((x message-dispatcher) out) (format out "~a" (string-show# (slot-value x 'message-dispatcher-ptr))))
(defmethod print-object ((x priority-queue) out) (format out "~a" (string-show# (slot-value x 'priority-queue-ptr))))
(defmethod print-object ((x processor) out) (format out "~a" (string-show# (slot-value x 'processor-ptr))))
(defmethod print-object ((x scheduler) out) (format out "~a" (string-show# (slot-value x 'scheduler-ptr))))
(defmethod print-object ((x signal) out) (format out "~a" (string-show# (slot-value x 'signal-ptr))))
(defmethod print-object ((x thread) out) (format out "~a" (string-show# (slot-value x 'thread-ptr))))

(defmethod print-object ((x time) out) (format out "~a" (string-show# (slot-value x 'time-ptr))))
(defmethod print-object ((x type) out) (format out "~a" (string-show# (slot-value x 'type-ptr))))
(defmethod print-object ((x event) out) (format out "~a" (string-show# (slot-value x 'event-ptr))))
(defmethod print-object ((x scheduler) out) (format out "~a" (string-show# (slot-value x 'scheduler-ptr))))

(defmethod print-object ((x device-audio-session) out) (format out "~a" (string-show# (slot-value x 'device-audio-session-ptr))))
(defmethod print-object ((x device-audio-stream) out) (format out "~a" (string-show# (slot-value x 'device-audio-stream-ptr))))
(defmethod print-object ((x device-audio) out) (format out "~a" (string-show# (slot-value x 'device-audio-ptr))))
(defmethod print-object ((x device-midi-session) out) (format out "~a" (string-show# (slot-value x 'device-midi-session-ptr))))
(defmethod print-object ((x device-midi-stream) out) (format out "~a" (string-show# (slot-value x 'device-midi-stream-ptr))))
(defmethod print-object ((x device-midi) out) (format out "~a" (string-show# (slot-value x 'device-midi-ptr))))
(defmethod print-object ((x device-file) out) (format out "~a" (string-show# (slot-value x 'device-file-ptr))))
(defmethod print-object ((x device-buffer) out) (format out "~a" (string-show# (slot-value x 'device-buffer-ptr))))


; etc

; ---------------------------------------------------------------------------------------------------

(defmacro thread-holding ((mutex &key (blocking t)) &rest form)
  (let* ((lock (gensym))
         (result (gensym))
         (lock-func (if blocking #'thread-lock #'thread-try-lock)))
    `(let* ((,lock ,mutex)
            (,result (funcall ,lock-func ,lock)))
            (cond (,result 
                   (progn
                     ,@form
                     (thread-unlock ,lock))) 
                  (t nil)))))
                                
; ---------------------------------------------------------------------------------------------------

; Aliases

(defmacro input-type (&rest args) `(processor-input-type ,@args))
(defmacro output-type (&rest args) `(processor-output-type ,@args))
(defmacro unary (&rest args) `(processor-unary ,@args))
(defmacro binary (&rest args) `(processor-binary ,@args))
(defmacro identity (&rest args) `(processor-identity ,@args))
(defmacro constant (&rest args) `(processor-constant ,@args))
(defmacro loop (&rest args) `(processor-loop ,@args))
(defmacro split (&rest args) `(processor-split ,@args))
(defmacro delay (&rest args) `(processor-delay ,@args))

; seq and par are binary, sequence and parallel are the reduced version
(defmacro seq (&rest args) `(processor-sequence ,@args))
(defmacro par (&rest args) `(processor-parallel ,@args))

(defun sequence (head &rest args)
  (cond
   (args (seq head (apply 'sequence args)))
   (t    head)))

(defun parallel (head &rest args)
  (cond
   (args (par head (apply 'parallel args)))
   (t    head)))

