

(progn
  (asdf:load-system :doremir))


(defvar *foreign-lib*)
(progn
  (push "/Users/hans/audio/build/Frameworks/" cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library '(:framework "DoReMIRAudio"))))

(close-foreign-library *foreign-lib*)

; ---------------------------------------------------------------------------------------------------

; Caveats:
;   - Calling a function on a destroyed object is undefined
;   - Calling (from-pointer) with the wrong type is undefined
;   - Calling a generic function on a type that does not implement a required interface is undefined

(in-package :doremir)

; For testing
(defvar x nil)
(defvar y nil)


(audioengine-initialize)
(audioengine-terminate)
(audioengine-set-log-file "/Users/hans/Library/Logs/ScoreCleaner/AudioEngine.log")
(audioengine-set-log-std)

; Ratio
; Audio Engine ratios are converted to Lisp ratios and vice versa
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
(ratio-create 1 2)
(ratio-succ (/ 1 2))
(ratio-recip (/ 567 235))

; String
; Audio Engine strings are converted to Lisp strings and vice versa
(setf x (string-empty))
(setf x (string-single 104))
(string-append (string-single 104) (string-dappend (string-single (char-int #\a)) (string-single 110)))
(string-append "hans " "höglund")
(string-length "högtalare")
(cl:print x)
;(string-destroy x)

; Pair
; Audio Engine pairs are NOT Lisp pairs
; They print as (1,2)
(setf x (pair-create 1 2))
(setf y (pair-copy x))
(pair-fst x)
(pair-snd x)
(pair-dup 3)
(pair-swap x)
(pair-assoc (pair-create 1 (pair-create 2 3)))
(pair-unassoc (pair-create (pair-create 1 2) 3))
(cl:print x)
(pair-destroy x)
(destroy x)

(pair-snd (from-pointer 'pair (to-pointer (pair-create 1 2))))
(pair-create (pair-create 1 2) (pair-create 3 4))
(pair-create (list-single 1) (set-single 2))

; List
; Audio Engine lists are NOT Lisp lists
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

; (export-list# (cl:list 1 2 (export-list# (cl:list 1 3 4))))
; (import-list# (list-cons 1 (list-cons 2 (list-single 3))))

; Set
(setf x (set-empty))
(setf x (set-single 1))
(setf x (set-add (random 20) x))
(setf x (set-remove (random 20) x))
(setf y (set-copy x))
(set-size x)
(set-is-empty x)
(set-is-single x)
(set-has 1 x)
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

; Map
(setf x (map-empty))
(setf x (map-add "name" "hans" x))
(setf x (map-add "skills" (list-cons 1 (list-empty)) x))
(setf x (map-remove "name" x))
(setf x (map-remove "skills" x))
; (setf x (map-remove (random 20) x))
(setf y (map-copy x))
(map-size x)
(map-is-empty x)
(map-is-single x)
; (map-get "name" x)
; (map-get "skills" x)
(setf x (map-add-entry (pair-create "surname" "höglund") x))
(setf x (map-remove-entry (pair-create "surname" "höglund") x))
; (map-has-key "name" x)
; (map-has-elem "hans" x)
; (map-has-entry (pair-create "surname" "höglund") x)
(map-is-submap-of x y)
(map-is-proper-submap-of x y)
; (map-sum x y)
; (map-product x y)
; (map-difference x y)
; (map-power x)
; (map-from-pair (pair-create 1 2))
; (map-from-list (list-single 1))
; (map-to-list x)
(cl:print x)
(map-destroy x)

; Buffer
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

; Midi
(setf x (midi-create-simple #x9 60 127))
(setf x (midi-create-sysex (buffer-create 1024)))
(setf y (midi-copy x))
(midi-is-simple x)
(midi-is-sysex x)
(midi-status x)
(midi-channel x)
(midi-simple-data x)
(pair-fst (midi-simple-data x))
(midi-sysex-data x)
(midi-destroy x)

; Time
; TODO macro like (make-time 2 :hours 3 :minutes)
(setf x (time-create 0 0 4 (rational 33.5))) ; days hours minutes seconds divs
(setf y (time-copy x))
(setf x (from-pointer 'time (add x y)))
(time-days x)
(time-hours x)
(time-minutes x)
(time-seconds x)
(time-divisions x)
(time-to-iso x)
(equal x y)
(destroy x)

; Types
(setf x (make-type :i8))
(setf x (make-type :i16))
(setf x (make-type :i32))
(setf x (make-type :i64))
(setf x (make-type :f32))
(setf x (make-type :f64))
(setf x (make-type :ptr))
(setf x (make-type '(:f32 . :f32)))
(setf x (make-type '(:pair :i8 :i32)))
(setf x (make-type '(:frame :f32)))
(setf x (make-type (make-type '(:vector :f32 1024))))
(setf x (make-type '((:f32 . :f32) . (:f32 . :f32))))
(setf x (make-type '(:f32 :f32 :f32 . :f32)))
(setf x (make-type '(:vector (:pair :i8 :i32) 24)))
(type-is-simple x)
(type-is-pair x)
(type-is-vector x)
(type-is-frame x)
(type-size-of 256 x)
(type-offset-of 256 x)
(type-align-of x)


; Processor
(defcallback add-i8 :char ((c ptr) (x :char))
  (+ x 1))
(defcallback add-f32 :float ((c ptr) (x :float))
  (+ x 1))
(defcallback add-i8-i8 :char ((c ptr) (x :float) (y :float))
  (+ x 1))
(setf x (processor-unary (make-type :i8) (make-type :i8) (callback add-i8) nil))
(setf x (processor-unary (make-type :f32) (make-type :f32) (callback add-f32) nil))
(setf x (processor-binary (make-type :i8) (make-type :i8) (make-type :i8) (callback add-i8-i8) nil))
(setf y x)

(setf x (processor-identity (make-type :i8)))
(setf x (processor-constant (make-type :i16) (make-type :i8) 0))
(setf x (processor-seq x y))
(setf x (processor-par x y))
(setf x (processor-loop x))
(setf x (processor-split (make-type '(:f32 . :f32))))

; (setf x (processor-delay i 44100))
(setf x (processor-add i))
(setf x (processor-subtract i))
(setf x (processor-multiply i))
(setf x (processor-power i))
(setf x (processor-divide i))
(setf x (processor-modulo i))
(setf x (processor-absolute i))
(setf x (processor-not i))
(setf x (processor-and i))
(setf x (processor-or i))
(setf x (processor-xor i))
(setf x (processor-bit i))
(setf x (processor-bit i))
(setf x (processor-bit i))
(setf x (processor-bit i))
(setf x (processor-shift i))
(setf x (processor-shift i))
(setf x (processor-equal i))
(setf x (processor-less-than i))
(setf x (processor-greater-than i))
(setf x (processor-less-than-equal i))
(setf x (processor-greater-than-equal i))
(setf x (processor-acos i))
(setf x (processor-asin i))
(setf x (processor-atan i))
(setf x (processor-cos i))
(setf x (processor-sin i))
(setf x (processor-tan i))
(setf x (processor-exp i))
(setf x (processor-log i))
(setf x (processor-log10 i))
(setf x (processor-pow i))
(setf x (processor-sqrt i))
(setf x (processor-abs i))
(setf x (processor-min i))
(setf x (processor-max i))
(setf x (processor-fmod i))
(setf x (processor-remainder i))
(setf x (processor-floor i))
(setf x (processor-ceil i))
(setf x (processor-rint i))


; Signal

; Message stuff

; Scheduler


; Devices
(device-audio-begin-session)
(device-audio-restart-session)
(device-audio-end-session)
(device-audio-with-session)
(device-audio-all)
(device-audio-default)
(device-audio-name)
(device-audio-host-name)
(device-audio-has-input)
(device-audio-has-output)
(device-audio-channels)
(device-audio-start-stream)
(device-audio-restart-stream)
(device-audio-stop-stream)
(device-audio-with-stream)

(device-midi-all)
(device-midi-default)
(device-midi-name)
(device-midi-host-name)
(device-midi-has-input)
(device-midi-has-output)
(device-midi-begin-session)
(device-midi-restart-session)
(device-midi-end-session)
(device-midi-start-stream)
(device-midi-restart-stream)
(device-midi-stop-stream)
(device-midi-with-session)
(device-midi-with-stream)

(device-file-create)
(device-file-destroy)
(device-file-run)

(device-buffer-create)
(device-buffer-destroy)
(device-buffer-run)


; Error
; check
; error-message
; error-module


; Priority queue
(setf x (priorityqueue-empty))
(priorityqueue-insert (random 1000) x)
(priorityqueue-peek x)
(priorityqueue-pop x)


; Atomic
(setf x (atomic-create))
(setf y (atomic-copy x))
(atomic-exchange x 0 1)      ; FIXME
(atomic-exchange x 1 0)
(atomic-get x)
(atomic-set x 1)
(atomic-add x 1)
; (atomic-modify (lambda (x) x) x)
(atomic-destroy x)

(setf x (atomic-queue-create))
(atomic-queue-destroy x)
(atomic-queue-write x (random 20))
(atomic-queue-read x)               ; FIXME <ptr 0> should be nil


; Doremir (generic functions)
(equal              x y)
(less-than          x y)
(greater-than       x y)
(less-than-equal    x y)
(greater-than-equal x y)
(min                x y)
(max                x y)
(add                x y)
(multiply           x y)
(subtract           x y)
(divide             x y)
(absolute           x)
(copy               x)
(destroy            x)
(string-show        x)
(string-to-json     x)
(string-from-json   x)



(equal              0 0)
(equal              0 1)
(less-than          0 1)
(greater-than       0 1)
(less-than-equal    0 1)
(greater-than-equal 0 1)
(min                0 0)
(max                0 1)
(add                0 0)
(multiply           0 1)
(subtract           0 0)
(divide             0 1)
(absolute           -3)
; copy
; destroy









