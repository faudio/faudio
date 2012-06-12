;;;-*- Mode: Lisp; Package: (:audio) -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  audio-util.lisp
;;
;;  Copyright (c) 2011 DoReMIR http://www.doremir.com
;;
;;  Author:  Hans Hoglund
;;  Created: 2011-10-27

(in-package :audio)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  #-audio-unicode
    (scl-uchars-to-string (fli:convert-to-foreign-string str :external-format :ascii)) 
  #+audio-unicode
    (fli:convert-to-foreign-string str :external-format :unicode)) 

(defun from-foreign-string (str)
  #-audio-unicode
    (fli:convert-from-foreign-string str :external-format :ascii)
  #+audio-unicode
    (fli:convert-from-foreign-string str :external-format :unicode))
                               

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
  (list `(let ((_value ,form))
     (if _value
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
       (push-list _pointers arr)
       (push-list _lengths size)
       arr)
     `(pop-list _lengths)))

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
  `(let ((_value ,form))
     (if (fli:null-pointer-p _value)
       nil
       (make-instance (intern-safe ,type :audio) :native-object _value))))

(defun import-enum (indices form)
    (setf cases nil)
    (setf numeral 0)
    (dolist (index indices)
      (push-list cases `(,numeral ,index))
      (setf numeral (1+ numeral)))
   `(ecase ,form ,@cases))

(defun import-list (type length form)
  `(let
    ((_list nil)
     (_array ,form))
    (dotimes (i (fli:dereference ,length))
      (push-list _list
        ,(import-form type length `(fli:dereference _array)))
      (fli:incf-pointer _array))
    (reverse _list)))                            
    
(defun import-list-2 (type length form)
  `(let
    ((_list nil)
     (_array ,form))
    (dotimes (i ,length)
      (push-list _list
        ,(import-form type length `(fli:dereference _array)))
      (fli:incf-pointer _array))
    (reverse _list)))


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
    (make-instance (intern-safe type :audio) :native-object pointer)))



(defun check-error (pair)
"If the caar of the given list is a non-null error pointer, signal a Lisp
error whose name is the car of the given list."
  (destructuring-bind (name ptr) pair
    (if (points-to-error-p ptr) 
      (common-lisp:error name :native-object (fli:dereference ptr)))))


; TODO remove _x names and make truly hygienic

;(defvar *native-call-lock* (mp:make-lock :name 'native-call-lock :important-p t :recursivep nil))

(defmacro native-call (function return-type (&rest arguments) &key errors)
"
This macro generates calls to native functions.

It should be able to generate efficient code for various kinds of signatures,
including different parameters, return values and errors.

Form
    name return-type ({argument}*)                    => return value
    name return-type ({argument}*) :errors ({error*}) => return value

    function      ::= function
    return-type   ::= type
    error         ::= symbol
    argument      ::= (form type)
    type          ::= :integer
                      :float
                      :double
                      :string
                      :atom
                      (:list type)
                      (:function)
                      (:enum {stringable}*)
    stringable    ::= keyword
                      symbol
"
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
                        (push-last _errors (list ',type error-pointer))
                    error-pointer))
            errors))

          (length-pointer-arguments
            (if list-return
              (list `(let ((length-pointer (make-integer-pointer)))
                       (setf _length length-pointer)
                     length-pointer))
              ()))

          (function-arguments
            (concatenate 'list standard-arguments
                               length-pointer-arguments
                               error-pointer-arguments)))

  `(progn
;     (mp:with-lock (*native-call-lock*)
       
     (midi::write-to-log-file :audio 
                              "audio::native-call: process: ~a function: ~a" 
                              (mp:process-name mp:*current-process*) 
                              ',function)
     
     (setf _errors nil)
     (setf _pointers nil)
     (setf _lengths nil)
     (setf _length nil)
     
     (setf _call-result
           (,function ,@function-arguments))
     (setf _result
             ,(import-form return-type '_length '_call-result))
     
     (dolist (err _errors)
       (check-error err))
     (dolist (ptr _pointers)
       (fli:free ptr))
     
     _result))
;)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmessage (name &rest fields)
"
Defines utility functions for the given message type.

This macro defines:
    (name &rest)        a constructor
    (name-p value)      a type predicate
    (name-bind pitch)   a destructor

Example:
    (defmessage note-on pitch velocity)

    (setf n (note-on 60 127))               ==> (:note-on 60 127)
    (note-on-p n)                           ==> t
    (note-on-p nil)                         ==> nil
    (note-on-bind n (list pitch velocity))  ==> (60 127)
"
  (let* ((name (string name))
         (tag (values (intern-safe name :keyword)))

         (constructor (intern-safe name))
         (predicate (intern-safe (format nil "~a-p" name)))
         (destructor (intern-safe (format nil "~a-bind" name)))

         (bindings (cons (gensym) fields))

         (args (gensym))
         (msg (gensym))
         (value (gensym))
         (body (gensym)))

  `(progn (defun ,constructor (&rest ,args) (cons ,tag ,args))
          (defun ,predicate (,msg) (and (listp ,msg) (eq ,tag (car ,msg))))
          (defmacro ,destructor (,value &body ,body)
            `(destructuring-bind ,',bindings ,,value (progn ,@,body))))))


(defmacro define-message-reader (field &rest tags)
"
Defines a shared reader method for the given message.

Usage:
    (define-message-reader pitch note-on note-off)
    (pitch (note-on 60 127))    ==> 60
    (pitch (note-off 60))       ==> 60
"
  (let* ((msg (gensym))
         (cases (mapcar (lambda (name)
              (let* ((name (string name))
                     (predicate (intern-safe (format nil "~a-p" name)))
                     (binder (intern-safe (format nil "~a-bind" name))))
                `((,predicate ,msg) (,binder ,msg ,field)))) tags)))
    `(defun ,field (,msg) (cond ,@cases (t nil)))))



