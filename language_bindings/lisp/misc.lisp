; 
; 
; 
; ;; 
; ;; Miscellaneous Lisp library functions
; ;;
; 
(in-package :misc)

;; Numbers

(defun succ (x)
"The same as (+ x 1)"
  (+ x 1))

(defun pred (x)
"The same as (- x 1)"
  (- x 1))


;; Conses

(defmacro bind-pair ((x y) p form)
"Binds the given variables to (x . y)"
  `(let* ((xs ,p)
          (,x (car xs))
          (,y (cdr xs)))
    ,form))

(defmacro bind-trip ((x y z) p form)
"Binds the given variables to (x y . z)"
  `(let* ((xs ,p)
          (,x (car xs))
          (,y (cadr xs))
          (,z (cddr xs)))
    ,form))

(defmacro bind-quad ((x y z a) p form)
"Binds the given variables to (x y z . a)"
  `(let* ((xs ,p)
          (,x (car xs))
          (,y (cadr xs))
          (,z (caddr xs))
          (,a (cdddr xs)))
    ,form))    

;; Lists

; (defun coerce-list (x)
; "Make x into a singleton list if it is not already a list"
;   (if (listp x) 
;     x 
;     (list x)))
; 
; (defun take (n xs &optional (m 0))
; "Removes all but the first n values of xs"
;   (if (> n m)
;     (cons (car xs) (split n (cdr xs) (succ m)))
;     nil))
; 
; (defun drop (n xs &optional (m 0))
; "Removes the first n values of xs"
;   (if (> n m)
;     (drop n (cdr xs) (succ m))
;     xs))        

; (defun split (n xs)
; "Splits the given list at n, returning a pair of elements before and after"
;   ((take n xs) . (drop n xs)))


;; Sequences

(defun seq-first (xs)
"Like first but works on sequences"
  (elt xs 0))
(defun seq-last (xs)
"Like last but works on sequences"
  (elt xs (dec (length xs))))

(defun seq-take (n xs)
  nil) ; TODO

(defun seq-drop (n xs)
  nil) ; TODO

(defun seq-split (n xs)
  nil) ; TODO

(defun retain-if (p xs)
  (remove-if-not (p xs)))

(defun retain-if-not (p xs)
  (remove-if (p xs)))

(defun partition (p xs)
"Returning a pair of lists with non-matching and matching elements"
  (list (remove-if p xs) (retain-if p xs)))

(defun mapconc (f xs)
  nil) ; TODO

(defun zip (&rest seqs)
  nil) ; TODO              

;; Strings

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))           


;; Option parser


(defun parse-flag (str)
"If the given arg is on the form -w or --w, return w.
Otherwise, return nil"
  (let* ((dashp 
            (lambda (str n)
              (char= (elt str n) #\-))))
    (if (> (length str) 1)
      (if (funcall dashp str 0)
        (if (funcall dashp str 1) 
          (subseq str 2)
          (subseq str 1))))))


(defun make-keyword (name) 
  (values (intern (string-upcase name) "KEYWORD")))

(defun smart-reader (str)
  (read-from-string (concatenate 'string "\"" str "\"")))

(defun parse-args (arglist &optional (reader 'smart-reader))
"Parse the given list of args, return a list on the form
(((:opt1 . val1) ... (optN . valN)) . (arg1 ... argN))"
  (setf last nil)
  (setf options nil)
  (setf arguments nil)
  (dolist (x arglist)
    (if last       
      (progn (push (cons last (funcall reader x)) options) (setf last nil))
      (if (parse-flag x) 
          (setf last (make-keyword (parse-flag x))) 
          (progn (push (funcall reader x) arguments) (setf last nil)))))
  (cons options (reverse arguments)))


(defmacro with-parsed-args (arglist form)
"Evaluates form in a context where the variables *options* and *arguments* are
bound to the output of (parse-args arglist)"
  (let ((__both__ (gensym)))
    `(let* ((,__both__    (parse-args ,arglist))
            (*options*    (car ,__both__))
            (*arguments*  (cdr ,__both__)))
      ,form))) 

(defmacro find-option (name)
"Returns the name of the given option, or nil if it does not exits.
Intended to be used inside with-parsed-args, so

  (with-parsed-args '('-foo' '1' '-bar' '2')
    (list
      (find-option :foo)
      (find-option :bar)
      (find-option :nope)))   
      ==> '(1 2 nil)
  
"
  `(cdr (assoc ,name *options*)))


;; System

(defun command-line-args ()
  (or 
   #+lispworks (cdddr system:*line-arguments-list*))) 

     

