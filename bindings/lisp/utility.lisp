
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(in-package :faudio)

#|
    High-level functions.
    
    The API define here should be considered experimental.
|#

; ---------------------------------------------------------------------------------------------------

(defgeneric find (predicate structure))
(defgeneric find-index (predicate structure))
(defgeneric filter (predicate structure))
(defgeneric map (function structure))
(defgeneric join (structure))
(defgeneric join-map (function structure))

; ---------------------------------------------------------------------------------------------------

(defgeneric size (b)) 
(defgeneric get (b i)) 
(defgeneric (setf get) (v b i)) 

; ---------------------------------------------------------------------------------------------------

(defmethod find (p (xs list))
  (list-find* p xs))

(defmethod find-index (p (xs list))
  (list-find-index* p xs))

(defmethod filter (p (xs list))
  (list-filter* p xs))

(defmethod map (p (xs list))
  (list-map* p xs))

(defmethod join ((xs list))
  (list-join xs))

(defmethod find (p (xs cl:list))
  (find p (export-list xs)))

(defmethod find-index (p (xs cl:list))
  (find-index p (export-list xs)))

(defmethod filter (p (xs cl:list))
  (filter p (export-list xs)))

(defmethod map (p (xs cl:list))
  (map p (export-list xs)))

(defmethod join ((xs cl:list))
  (join (export-list xs)))

; ---------------------------------------------------------------------------------------------------

; (defmethod filter (p (xs event))
;   (event-filter* p xs))
; 
; (defmethod map (p (xs event))
;   (event-map* p xs))


; ---------------------------------------------------------------------------------------------------

(defmethod size ((b buffer))
  (buffer-size b))

(defmethod get ((b buffer) i)
  (buffer-get b i))

(defmethod (setf get) (v (b buffer) i)
  (buffer-set b i v))

(defun buffer-to-array (x)
  (setf sz (buffer-size x))
  (setf a (make-array sz :element-type 'unsigned-byte))
  (dotimes (i sz)
    (setf (aref a i) (get x i)))
  a)

(defun array-to-buffer (a)
  (setf sz (array-total-size a))
  (setf x (buffer-create sz))
  (dotimes (i sz)
    (setf (get x i) (aref a i)))
  x)

(defun buffer-read-audio* (path)
  (let ((res (buffer-read-audio path)))
    (when (error-check res)
      (error-log nil (to-error res))
      (cl:error (error-message (to-error res))))
    res))

; ---------------------------------------------------------------------------------------------------

; Macro version of function pair thread-*-lock, thread-unlock

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

(defcfun (signal-loop "fa_signal_loop") signal (a unary) (b ptr))
(defcallback signalcall1# ptr ((f ptr) (x ptr))
  (funcall (int-to-func# f) (from-pointer 'signal x)))
(defun signal-loop* (f)
  (signal-loop (callback signalcall1#) (func-to-int# f)))

(defun is-signal (x) 
  (cl:equal (type-of x) 'signal))

(defun number-or-signal (x) 
  (if (is-signal x)
    x
    (signal-constant (coerce x 'double-float))))
(defun wrap-signal  (x) `(number-or-signal ,x))

(defmacro constant (&rest args) `(signal-constant (coerce ,@args 'double-float)))
(defmacro stime (&rest args) `(signal-time))
(defmacro counter (&rest args) `(signal-counter))
(defmacro random (&rest args) `(signal-random))
(defmacro input (n &rest args) `(signal-input ,n))
(defmacro output (n c &rest args) `(signal-output ,n ,c ,@(mapcar 'wrap-signal args)))
(defmacro + (&rest args) `(signal-add ,@(mapcar 'wrap-signal args)))
(defmacro * (&rest args) `(signal-multiply ,@(mapcar 'wrap-signal args)))
(defmacro sin (&rest args) `(signal-sin ,@(mapcar 'wrap-signal args)))
(defmacro cos (&rest args) `(signal-cos ,@(mapcar 'wrap-signal args)))

(defmacro delay (n &rest args) `(signal-delay ,n ,@(mapcar 'wrap-signal args)))
(defmacro line (&rest args) `(signal-line (coerce ,@args 'double-float)))
(defmacro loop (&rest args) `(signal-loop (coerce ,@args 'double-float)))
(defmacro input (&rest args) `(signal-input ,@args))



(defun duplicate (x) (cl:list x x))
(defun signal-run-default (proc &key (stream-callback (lambda (x) nil))) 
  (let* ((s (audio-begin-session))
       (i (audio-default-input s))
       (o (audio-default-output s))
       (st (audio-open-stream* i o proc)))
  (funcall stream-callback st)
  (capi:popup-confirmer nil "Running signal..."
    :callback-type :none 
    :ok-button "Stop" 
    :no-button nil 
    :cancel-button nil 
    :value-function #'(lambda (dummy) t))
  (destroy st)
  (destroy s))) 



(defun signal-print* (n x &key (controls '()))
  (let* ((buffer (signal-run-buffer n controls x)))
    (dotimes (i (/ (size buffer) 8)) 
      (cl:print (coerce 
                 (buffer-get-double buffer i) 
                 'single-float)))
    (destroy buffer)))




(defcallback signal-counter-add1 signal ((_ :pointer) (x signal))
  (signal-add x (constant 1)))

(defun signal-counter ()
  (signal-add 
    (signal-loop (callback signal-counter-add1) (cffi:null-pointer))
    (constant -1)))



