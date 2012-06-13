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

(defun string-replace (str1 sub1 sub2)
  (let ((str1 (string str1)) (str2 "") (sub1 (string sub1)) (sub2 (string sub2)) (index1 0))
    (loop
      (if (string-equal str1
                        sub1
                        :start1
                        index1
                        :end1
                        (min (length str1) (+ index1 (length sub1))))
          (progn (setq str2 (concatenate 'string str2 sub2)) (incf index1 (length sub1)))
        (progn
          (setq str2 (concatenate 'string str2 (subseq str1 index1 (1+ index1))))
          (incf index1)))
      (unless (< index1 (length str1)) (return str2)))))

(defun expand-path (path)    
  path) 
  ; (expand-current-directory (expand-home-directory path))) 

(defun expand-current-directory (path)
  (if (char= (seq-first path) #\.)
    (concatenate 'string
      (namestring (working-directory))
      (subseq path 0))
    path)) 
  ; (string-replace path "." (namestring (working-directory)))) ; FIXME

(defun expand-home-directory (path)
  (string-replace path "~" (environment-variable "HOME")))

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

(defun working-directory ()
  #+lispworks (hcl:get-working-directory))

(defun environment-variable (name &optional default)
    #+cmu
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-cmu
    (or
     #+allegro (sys:getenv name)
     #+clisp (ext:getenv name)
     #+ecl (si:getenv name)
     #+sbcl (sb-unix::posix-getenv name)
     #+lispworks (lispworks:environment-variable name)
     default))


     
;;; TODO actually use split-sequence

(macrolet ((check-bounds (sequence start end)
             (let ((length (gensym (string '#:length))))
               `(let ((,length (length ,sequence)))
                  (check-type ,start unsigned-byte "a non-negative integer")
                  (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
                  (unless ,end
                    (setf ,end ,length))
                  (unless (<= ,start ,end ,length)
                    (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end))))))

  (defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                         (count nil) (remove-empty-subseqs nil)
                         (test #'eql) (test-not nil) (key #'identity))
    "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (cond
      ((and (not from-end) (null test-not))
       (split-from-start (lambda (sequence start)
                           (position delimiter sequence :start start :key key :test test))
                         sequence start end count remove-empty-subseqs))
      ((and (not from-end) test-not)
       (split-from-start (lambda (sequence start)
                           (position delimiter sequence :start start :key key :test-not test-not))
                         sequence start end count remove-empty-subseqs))
      ((and from-end (null test-not))
       (split-from-end (lambda (sequence end)
                         (position delimiter sequence :end end :from-end t :key key :test test))
                       sequence start end count remove-empty-subseqs))
      ((and from-end test-not)
       (split-from-end (lambda (sequence end)
                         (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                       sequence start end count remove-empty-subseqs))))

  (defun split-sequence-if (predicate sequence &key (start 0) (end nil) (from-end nil)
                            (count nil) (remove-empty-subseqs nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs)))

  (defun split-sequence-if-not (predicate sequence &key (count nil) (remove-empty-subseqs nil)
                                (from-end nil) (start 0) (end nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if-not predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if-not predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs))))

(defun split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (loop
     :for right := end :then left
     :for left := (max (or (funcall position-fn sequence right) -1)
                       (1- start))
     :unless (and (= right (1+ left))
                  remove-empty-subseqs) ; empty subseq we don't want
     :if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
       :return (values (nreverse subseqs) right)
     :else
       :collect (subseq sequence (1+ left) right) into subseqs
       :and :sum 1 :into nr-elts
     :until (< left start)
   :finally (return (values (nreverse subseqs) (1+ left)))))

(defun split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (let ((length (length sequence)))
    (loop
       :for left := start :then (+ right 1)
       :for right := (min (or (funcall position-fn sequence left) length)
                          end)
       :unless (and (= right left)
                    remove-empty-subseqs) ; empty subseq we don't want
       :if (and count (>= nr-elts count))
       ;; We can't take any more. Return now.
         :return (values subseqs left)
       :else
         :collect (subseq sequence left right) :into subseqs
         :and :sum 1 :into nr-elts
       :until (>= right end)
     :finally (return (values subseqs right)))))
