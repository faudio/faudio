

(progn
  (asdf:load-system :doremir))

(progn
  (push "/Users/hans/audio/build/Frameworks/" cffi:*darwin-framework-directories*)
  (cffi:load-foreign-library '(:framework "DoReMIRAudio")))

; ---------------------------------------------------------------------------------------------------

(in-package :doremir)

(audioengine-initialize)
(audioengine-terminate)
(audioengine-set-log-file "/Users/hans/Librar/Logs/ScoreCleaner/AudioEngine.log")
(audioengine-set-log-std)

; For testing
(defvar x nil)

(setf x (buffer-create 1024))
(setf x (buffer-resize 2048 x))
(buffer-size x)
(buffer-peek x 1)
(buffer-poke x 1 10)
(dotimes (i 1024)
  (buffer-poke x i (mod i 256)))
(dotimes (i 1024)
  (buffer-poke x i 0))
(cl:print x)
(buffer-destroy x)

; Audio Engine pairs are not Lisp pairs
(setf x (pair-create 1 2))
(setf x (pair-copy x))
(pair-fst x)
(pair-snd x)
(pair-dup 3)
(pair-swap x)
(pair-assoc (pair-create (pair-create 1 2) 3))
(pair-unassoc (pair-create 1 (pair-create 2 1)))
(cl:print x)
(pair-destroy x)


; Audio Engine lists are not Lisp lists
; They print as [1,2,3..]
(setf x (list-empty))
(setf x (list-single 0))
(setf x (list-cons 1 x))
(setf x (list-cons (random 20) x))
(setf x (list-tail x))
(setf y (list-copy x))
(list-is-empty x)
(list-is-single x)
(list-length x)
(list-head x)
(list-tail x)
(list-init x)
(list-last x)
(list-append x x)
(list-reverse x)
;(list-sort x)
(list-take 5 x)
(list-drop 5 x)
(list-index 2 x)
(list-range 2 3 x)
(list-insert 2 23 x)
(list-remove 2 x)
(list-insert-range 2 (list-single 56) x)
(list-remove-range 2 3 x)
(list-has 1 x)
; (list-find (lambda (x) t) x)
(list-index-of 1 x)
; (list-find-index (lambda (x) t) x)
; (list-filter (lambda (x) t) x)
; (list-map (lambda (x) (+ 1 x)) x)
; (list-fold-left (lambda (x y) (+ x y) nil 0 x)
; (list-concat x)
(cl:print x)
(list-destroy x)

(setf x (set-empty))
(setf x (set-single (from-int8 1)))
(setf x (set-add (from-int8 (random 20)) x))
(setf x (set-remove (from-int8 (random 20)) x))
(setf y (set-copy x))
(set-size x)
(set-is-empty x)
(set-is-single x)
(set-has (from-int8 1) x)
(set-is-subset-of y x)
(set-is-proper-subset-of y x)
(set-sum x y)
(set-product x y)
(set-difference x y)
; (set-power x)
; (set-from-list (list-empty))
(set-to-list x)
(cl:print x)
(set-destroy)

(setf x (map-empty))
(setf x (map-add "name" "hans" x))
(setf x (map-add "skills" (list-empty) x))
(setf x (map-remove "name" x))
(setf x (map-remove "skills" x))
; (setf x (map-remove (from-int8 (random 20)) x))
(setf y (map-copy x))
(map-size x)
(map-is-empty x)
(map-is-single x)
; (map-get "name" x)
; (map-get "skills" x)
(map-add-entry)
(map-remove-entry)
(map-has-key)
(map-has-elem)
(map-has-entry)
(map-is-submap-of)
(map-is-proper-submap-of)
(map-sum)
(map-product)
(map-difference)
(map-power)
(map-from-pair)
(map-from-list)
(map-to-list)
(cl:print x)
(map-destroy)

; Audio Engine ratios are converted to Lisp ratios
(setf x (ratio-create 1 2))
(setf y (ratio-create 278 12))
(ratio-num x)
(ratio-denom x)
(ratio-add x y)
(ratio-subtract x y)
(ratio-multiply x y)
(ratio-divide x y)
;(ratio-remainder x y)
(ratio-succ x)
(ratio-pred x)
(ratio-negate x)
(ratio-recip x)
(cl:print x)
(ratio-destroy x)

; test auto-convert
(doremir::ratio-create 1 2)
(doremir::ratio-succ (/ 1 2))
(doremir::ratio-recip (/ 567 235))

;(denominator (/ 1 2))

; Audio Engine strings are converted to Lisp strings
(setf x (string-empty))
(setf x (string-single 104))
(string-append (string-single 104) (string-dappend (string-single 97) (string-single 110)))
(string-append "hans " "höglund")
(string-length "högtalare")
(cl:print x)
;(string-destroy x)

(setf x (midi-create-simple #x9 60 127))
(setf x (midi-create-sysex (buffer-create 1024)))
(setf x (midi-copy x))
(midi-status x)
(midi-channel x)
(midi-is-simple x)
(midi-simple-data x)
(pair-fst (midi-simple-data x))
(midi-is-sysex x)
(midi-sysex-data x)
(midi-destroy x)

(setf x (atomic-create))
(setf y (atomic-copy x))
(atomic-exchange x (from-int8 0) (from-int8 1))
(atomic-exchange x (from-int8 1) (from-int8 0))
(to-int8 (atomic-get x))
(atomic-set x (from-int8 0))
(atomic-add x (from-int8 1))
; (atomic-modify (lambda (x) x) x)
(atomic-destroy x)

(setf x (atomic-queue-create))
(atomic-queue-destroy x)
(atomic-queue-write x (from-int8 (random 20)))
(atomic-queue-read x)
(to-int8 (atomic-queue-read x))

(eq
 (type-of (fli:allocate-foreign-object :type :long))
 (type-of (from-int8 0)))

(less-than-equal 2 2)
(is-bool nil)


(equal              (from-int8 0) (from-int8 0))
(equal              (from-int8 0) (from-int8 1))
(less-than          (from-int8 0) (from-int8 1))
(greater-than       (from-int8 0) (from-int8 1))
(less-than-equal    (from-int8 0) (from-int8 1))
(greater-than-equal (from-int8 0) (from-int8 1))
(min                (from-int8 0) (from-int8 0))
(max                (from-int8 0) (from-int8 1))
(add                (from-int8 0) (from-int8 0))
(multiply           (from-int8 0) (from-int8 1))
(subtract           (from-int8 0) (from-int8 0))
(divide             (from-int8 0) (from-int8 1))
(absolute           (from-int8 -3))
; copy
; destroy









