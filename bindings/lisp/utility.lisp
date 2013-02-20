
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(in-package :audio-engine)

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

(defmethod size ((b buffer))
  (buffer-size b))

(defmethod get ((b buffer) i)
  (buffer-get b i))

(defmethod (setf get) (v (b buffer) i)
  (buffer-set b i v))

; ---------------------------------------------------------------------------------------------------

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


; (defmacro < (&rest args) `(greater-than ,@args))
; (defmacro > (&rest args) `(less-than ,@args))
; (defmacro <= (&rest args) `(less-than-equal ,@args))
; (defmacro >= (&rest args) `(less-than-equal ,@args))
; (defmacro + (&rest args) `(add ,@args))
; (defmacro - (&rest args) `(subtract ,@args))
; (defmacro * (&rest args) `(multiply ,@args))
; (defmacro / (&rest args) `(divide ,@args))
; (defmacro abs (&rest args) `(absolute ,@args))








