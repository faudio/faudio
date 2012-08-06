;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)

(push :lispworks *backends*)

(defun lispworks-load-library ()
  (fli:register-module 
    :scorecleaneraudio
    :real-name
      #+:win32 "sclaudio"
      #+:macosx "libsclaudio.dylib"  
; FIXME should NOT be hardcoded
;      #+:macosx "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
    :connection-style :immediate))

(defun lispworks-unload-library ()
  (fli:disconnect-module
    :scorecleaneraudio
    :remove t))

(defun lispworks-call (func type args errors)
  (lispworks-call-impl func type args errors)) 

(defconstant +intern-to-package+ :audio-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
  TODO                             
    - Make macros hygienic (to get rid of the cascades of compiler warnings)
    - Check allocation of atoms etc
    - Optimize options passing etc using (with-dynamic-foreign-objects) ?

|#

(defun always-list (x)
"If the given value is a list it is returned, otherwise a singleton 
list containing the given value is returned."
  (if (listp x) x (list x)))

(defun headp (list elem)
"Whether the first given value is a list whose car is the second value."
  (and (listp list) (eq elem (car list))))

(defmacro push-list (list elem)
  `(setf ,list (cons ,elem ,list)))

(defmacro push-last (list elem)
  `(setf ,list (append ,list (list ,elem))))

(defmacro peek-list (list)
  `(car ,list))

(defmacro pop-list (list)
  `(progn
     (setf elem (car ,list))
     (setf ,list (cdr ,list))
     elem))

(defmacro while-pop-list (list name forms)
  (let ((dummy (gensym)))
    `(do ((,dummy 0 (pop-list ,list))) 
      ((eq nil ,list)) 
        (let ((,name (car ,list))) (progn ,forms)))))

(defmacro intern-safe (name &rest package)
  `(intern (string-upcase ,name) ,@package))

(defmacro intern-qualified (prefix symbol &rest package)
  `(intern-safe (concatenate 'string (string prefix) (string symbol)) ,@package))

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun partially (f &rest args)
  (lambda (x) (apply f (nconc args (list x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-error-pointer ()
    (let ((error-ptr (fli:allocate-foreign-object :type '(:pointer :void))))
         (setf (fli:dereference error-ptr) fli:*null-pointer*)
          error-ptr))

(defun points-to-error-p (error-ptr)
  (not (fli:null-pointer-p (fli:dereference error-ptr))))

(defun make-integer-pointer ()
    (fli:allocate-foreign-object :type :int))

(defun make-object-array (size)
    (fli:allocate-foreign-object :pointer-type :void :nelems size))

(defun to-foreign-string (str)
  ; #-audio-unicode
    ; (scl-uchars-to-string (fli:convert-to-foreign-string str :external-format :ascii)) 
  ; #+audio-unicode
    (fli:convert-to-foreign-string str :external-format :unicode :null-terminated-p t)) 

(defun from-foreign-string (str)
  ; #-audio-unicode
    ; (fli:convert-from-foreign-string str :external-format :ascii)
  ; #+audio-unicode
    (fli:convert-from-foreign-string str :external-format :unicode :null-terminated-p t))
                               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun void-type-p (type) (eq :void type))
(defun integer-type-p (type) (eq :integer type))
(defun float-type-p (type) (eq :float type))
(defun double-type-p (type) (eq :double type))
(defun boolean-type-p (type) (eq :boolean type))
(defun string-type-p (type) (eq :string type))
(defun atom-type-p (type) (eq :atom type))

(defun enum-type-p (type)
  (headp type :enum))

(defun list-type-p (type)
  (headp type :list))

(defun function-type-p (type)
  (headp type :function))

(defun list-list-type-p (type)
  (and (headp type :list)
       (headp (cdr type) :list)))

(defun get-fli-type (type)
  (case type
    (:integer  :int)
    (:double   :double)
    (:float    :float)
    (:string   'sclstring)
    (otherwise '(:pointer :void))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun export-integer (form) (list form))
(defun export-float (form) (list form))
(defun export-double (form) (list form))
(defun export-boolean (form) (list `(if ,form 1 0)))
(defun export-string (form) 
  (list `(to-foreign-string ,form)))

; TODO free the string pointer after invocation (as it is copied in C++)


(defun export-atom (form)
  (list `(typecase ,form
    (integer (scl-atom-from-int ,form))
    (double-float (scl-atom-from-double ,form))
    (string (scl-atom-from-string (to-foreign-string ,form))))))

(defun export-object (type form)
  (list `(let ((__value__ ,form))
     (if __value__
       (slot-value ,form 'native-object)
       fli:*null-pointer*))))

(defun export-enum (indices form)
   (setf cases nil)
   (setf numeral 0)
   (dolist (index indices)
     (push-list cases `(,index ,numeral))
     (setf numeral (1+ numeral)))
  (list `(ecase ,form ,@cases)))

(defun export-list (type form)
   (list
     `(let* ((lst ,form)
             (size (length lst))
             (fli-type (get-fli-type ,type))
             (arr (fli:allocate-foreign-object :type fli-type :nelems size))
             (ptr (fli:copy-pointer arr)))
       (dolist (elem lst)
         (setf (fli:dereference ptr)
           ,(car (export-form type 'elem)))
         (fli:incf-pointer ptr))
       (push-list __pointers__ arr)
       (push-list __lengths__ size)
       arr)
     `(pop-list __lengths__)))

(defun export-function (type form) (list form))


(defun import-void (form) nil)
(defun import-integer (form) form)
(defun import-float (form) form)
(defun import-double (form) form)
(defun import-boolean (form) `(not (eq 0 ,form)))
(defun import-string (form) 
  `(from-foreign-string ,form))

(defun import-atom (form)
  `(case (scl-atom-type ,form)
        (0 (scl-atom-to-int ,form))
        (1 (scl-atom-to-double ,form))
        (2 (from-foreign-string (scl-atom-to-string ,form)))))

; TODO fetch index lazily (but not at compile time) instead of hardcoding

(defun import-object (type form)
  `(let ((__value__ ,form))
     (if (fli:null-pointer-p __value__)
       nil
       (make-instance (intern-safe ,type +intern-to-package+) :native-object __value__))))

(defun import-enum (indices form)
    (setf cases nil)
    (setf numeral 0)
    (dolist (index indices)
      (push-list cases `(,numeral ,index))
      (setf numeral (1+ numeral)))
   `(ecase ,form ,@cases))

(defun import-list (type length form)
  `(let
    ((__list__ nil)
     (__array__ ,form))
    (dotimes (i (fli:dereference ,length))
      (push-list __list__
        ,(import-form type length `(fli:dereference __array__)))
      (fli:incf-pointer __array__))
    (reverse __list__)))                            
    
(defun import-list-2 (type length form)
  `(let
    ((__list__ nil)
     (__array__ ,form))
    (dotimes (i ,length)
      (push-list __list__
        ,(import-form type length `(fli:dereference __array__)))
      (fli:incf-pointer __array__))
    (reverse __list__)))


(defun export-form (type form)
  (cond ((integer-type-p type)
          (export-integer form))
        ((float-type-p type)
          (export-float form))
        ((double-type-p type)
          (export-double form))
        ((boolean-type-p type)
          (export-boolean form))
        ((string-type-p type)
          (export-string form))
        ((atom-type-p type)
          (export-atom form))
        ((enum-type-p type)
          (export-enum (cdr type) form))
        ((list-type-p type)
          (export-list (second type) form))
        ((function-type-p type)
          (export-function (second type) form))
        (t (export-object type form))))

(defun import-form (type length form)
  (cond ((void-type-p type)
          (import-void form))
        ((integer-type-p type)
          (import-integer form))
        ((float-type-p type)
          (import-float form))
        ((double-type-p type)
          (import-double form))
        ((boolean-type-p type)
          (import-boolean form))
        ((string-type-p type)
          (import-string form))
        ((atom-type-p type)
          (import-atom form))
        ((enum-type-p type)
          (import-enum (cdr type) form))
        ((list-type-p type)
          (import-list (second type) length form))
        ; ((function-type-p type)
        ;   (import-function (second type) form))
        (t (import-object type form))))

#|
(defun convert-list (type length array)
  (setf res nil)
  (dotimes (i length)
    (push-list res (eval (import-form type length (fli:dereference array))))
    (fli:incf-pointer array))
  (reverse res))
|#

(defun convert-object (type pointer)
  (if (fli:null-pointer-p pointer)
    nil
    (make-instance (intern-safe type +intern-to-package+) :native-object pointer)))



(defun check-error (pair)
"If the caar of the given list is a non-null error pointer, signal a Lisp
error whose name is the car of the given list."
  (destructuring-bind (name ptr) pair
    (if (points-to-error-p ptr) 
      (common-lisp:error name :native-object (fli:dereference ptr)))))



(defun lispworks-call-impl (function return-type arguments errors)
    (let*  ((list-return
              (list-type-p return-type))

            (nested-list-return
              (list-list-type-p return-type))

            (standard-arguments
              (apply 'append
                (mapcar
                  (lambda (arg) (destructuring-bind (form type) arg (export-form type form)))
                arguments)))

            (error-pointer-arguments
              (mapcar (lambda (type)
                       `(let ((error-pointer (make-error-pointer)))
                          (push-last __errors__ (list ',type error-pointer))
                      error-pointer))
              errors))

            (length-pointer-arguments
              (if list-return
                (list `(let ((length-pointer (make-integer-pointer)))
                         (setf __length__ length-pointer)
                       length-pointer))
                ()))

            (function-arguments
              (concatenate 'list standard-arguments
                                 length-pointer-arguments
                                 error-pointer-arguments)))
    `(progn
       ; log?
       (setf __errors__ nil)
       (setf __pointers__ nil)
       (setf __lengths__ nil)
       (setf __length__ nil)

       (setf __call-result__
             (,function ,@function-arguments))
       (setf __result__
               ,(import-form return-type '__length__ '__call-result__))

       (dolist (err __errors__)
         (check-error err))
       (dolist (ptr __pointers__)
         (fli:free ptr))

       __result__)
       ))
