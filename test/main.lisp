

(progn
  (asdf:load-system :doremir))

(progn 
  (push "/Users/hans/audio/build/Frameworks/" cffi:*darwin-framework-directories*)
  (load-foreign-library '(:framework "DoReMIRAudio"))) 

; ---------------------------------------------------------------------------------------------------

(in-package :doremir)

(setf x (buffer-create 1024))
(setf x (buffer-resize 2048 x))
(buffer-size x)
(buffer-peek x 1)
(buffer-poke x 1 10)
(dotimes (i 1024)
  (buffer-poke x i (mod i 256)))
(cl:print x)
(buffer-destroy x)

; Audio Engine lists are not Lisp lists
; They print as [1,2,3..]
(setf x (list-empty))
(setf x (list-single (from-int8 0)))
(setf x (list-cons (from-int8 1) x))
(setf x (list-cons (from-int8 (random 20)) x))
(setf x (list-tail x))
(list-is-empty x)
(list-is-single x)
(list-length x)
(list-head x)
(list-tail x)
(list-init x)
(list-last x)
(list-append x x)
(list-reverse x)
(list-sort x)
(list-take 5 x)
(list-drop 5 x)
(list-index 2 x)
(list-range 2 3 x)
(list-insert 2 (from-int8 23) x)
(list-remove 2 x)
(list-insert-range 2 (list-single (from-int8 56)) x)
(list-remove-range 2 3 x)
(list-has (from-int8 1) x)
;(list-find (lambda (x) t) x)
(list-index-of (from-int8 1) x)
;(list-find-index (lambda (x) t) x)
; (list-filter (lambda (x) t) x)
; (list-map (lambda (x) (+ 1 x)) x)
; (list-fold-left (lambda (x y) (+ x y) nil (from-int8 0) x)
(list-concat x)
(cl:print x)
(list-destroy x)

; Audio Engine ratios are not Lisp ratios
(setf x (ratio-create 1 2))
(cl:print x)
(ratio-destroy x)

(setf x (string-empty))
(setf x (string-single 104))
(string-append (string-single 104) (string-dappend (string-single 97) (string-single 110)))
(string-append "hans " "höglund")
(string-length "högtalare")
(cl:print x)
(string-destroy x)



; (less-than-equal (slot-value x 'buffer-ptr) (slot-value x 'buffer-ptr))
; (less-than-equal (slot-value x 'list-ptr) (slot-value x 'list-ptr))
