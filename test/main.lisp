
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#


#|
    Caveats:
      - Calling a function on a destroyed object is undefined
      - Calling (from-pointer) with the wrong type is undefined
      - Calling a generic function on a type that does not implement a required interface is undefined
|#

(in-package :audio-engine)

; ---------------------------------------------------------------------------------------------------
; Top-level functions
; ---------------------------------------------------------------------------------------------------
;
; Doremir
; 
; These generic functions work for most types, see the "Implements" section in each
; relevant module documentation entry.
;
; Note that there is no multiple dispatch in this system: implementations assume
; that the given arguments are of the same type and dispatches on the leftmost
; argument. If you try to compare, say an integer and a list you will crash.
;
; Note: 
;   We could add runtime checks to fix this, but that would slow down certain operations.
;   Let me know if this is a problem. /Hans


(setf x 1)
(setf y 2)

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
(string-to-string   x)
(string-to-json     x)
(from-pointer 
 'map 
 (string-from-json "{\"foo\":null, \"bar\":[1,2,3]}"))


; ---------------------------------------------------------------------------------------------------
;
; Doremir.AudioEngine

(audioengine-initialize)
(audioengine-terminate)
(audioengine-set-log-file "/Users/hans/Library/Logs/DoReMIRAudio.log")
(audioengine-set-log-std)

(audioengine-log-info "What to say?")
(audioengine-log-warning "Beware!")
(audioengine-log-error "Rattlesnakes!")
(audioengine-log-error-from "Rattlesnakes!" "Test.Lisp")


; ---------------------------------------------------------------------------------------------------
;
; Doremir.Error

(setf x (error-create-simple 2 "An error" "From.Here"))
(destroy x)

(error-check nil) ; nil
(error-check x)   ; t
(error-log nil (to-error x))

(error-message (to-error x))
(error-severity (to-error x))
(error-origin (to-error x))

; ---------------------------------------------------------------------------------------------------
;
; Doremir.Dynamic

(dynamic-get-type nil)
(dynamic-get-type t)
(dynamic-get-type 123)
(dynamic-get-type 3.14159265)
; (dynamic-get-type 6.02214129d23)
(dynamic-get-type (pair-create 1 2))
(dynamic-get-type (list-empty))
(dynamic-get-type (set-empty))
(dynamic-get-type (map-empty))
(dynamic-get-type "foo")



; ---------------------------------------------------------------------------------------------------
; Data structures
; ---------------------------------------------------------------------------------------------------
;
; Doremir.Ratio
;
; AE ratios are interchangable with Lisp rationals in all contexts.

(setf x (ratio-create 1 2))
(setf y (ratio-create 278 12))
(ratio-destroy x)

(ratio-num x)
(ratio-denom x)

; Faster than the generic versions in the Doremir module
(ratio-add x y)
(ratio-subtract x y)
(ratio-multiply x y)
(ratio-divide x y)
(ratio-succ x)
(ratio-pred x)
(ratio-negate x)
(ratio-recip x)

; Mixing AE ratios and Lisp rationals
(ratio-create 1 2)
(ratio-succ (/ 1 2))
(ratio-recip (/ 567 235))

; ---------------------------------------------------------------------------------------------------
;
; Doremir.String
;
; AE strings are interchangable with Lisp strings in all contexts.
;
; Note that LispWorks does not support surrogate pairs, so the full Unicode range is not available.

(setf x (string-empty))
(setf x (string-single 104))
(setf x (string-repeat 30 (char-int #\a)))
(destroy x)

(string-append (string-single 104)
               (string-dappend (string-single (char-int #\a))
                               (string-single 110)))
(string-append "hans " "h�glund")
(string-length "h�gtalare")
(code-char (string-char-at 0 "foo"))
(string-matches "a*b*c+" "aaacc")

; (string-map* (lambda (c) c) "abcd")
; (string-join-map* (lambda (c) "a") "abcd")


; ---------------------------------------------------------------------------------------------------
;
; Doremir.Pair
;
; AE pairs are distinct from Lisp conses:
;
;   * AE pairs print as (1,2), not as '(1 . 2).
;   * You can pass Lisp conses to functions expecting AE pairs, but not the other way around.
;   * You can use (import-pair) and (export-pair) to convert, see example below.

(setf x (pair-create 1 2))
(setf y (pair-copy x))
(destroy x)

(pair-fst x)
(pair-snd x)
(pair-dup 3)
(pair-swap x)

(pair-assoc (pair-create 1 (pair-create 2 3)))
(pair-unassoc (pair-create (pair-create 1 2) 3))
(pair-snd (from-pointer 'pair (to-pointer (pair-create 1 2))))

(pair-create (pair-create 1 2) (pair-create 3 4)) ; Nested pairs are possible
(pair-create (list-single 1) (set-single 2))      ; Pairs containing other structures too

; Conversion
(import-pair (pair-create 1 2)))
(export-pair '(1 . 2))

; ---------------------------------------------------------------------------------------------------

; Doremir.List

; AE lists are distinct from Lisp lists:
;
;   * AE lists print as [1,2,3] or [] for the empty list, not as '(1 2 3) or '().
;   * You can pass Lisp lists to functions expecting AE lists, but not the other way around.
;   * You can use (import-list) and (export-list) to convert, see example below.

(setf x (list-empty))
(setf x (list-single 0))
(setf x (list-dcons 1 x))
(setf x (list-dcons (random 20) x))
(setf x (list-dtail x))
(setf x (export-list '(1 2 3 4)))
(setf y (list-copy x))
(destroy x)

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
(list-insert 2 23 x)
(list-remove 2 x)
(list-insert-range 2 (list-single 56) x)
(list-remove-range 2 3 x)

(list-has 1 x)
(list-index-of 1 x)
(list-join (list-empty))
(list-join (list-single (list-single 1)))


; Mixing AE lists and Lisp lists

(list-append '(1 2 3) (list-single 4))
(list-dcons 1 '())
(list-is-empty '())
(list-is-single '(1))

; Conversion

(export-list '(1 2 3))
(import-list (list-cons 1 (list-cons 2 (list-cons 3 (list-empty)))))


; Higher-order functions
;
;   list-find, list-map etc are unwrapped C functions and take callbacks
;   list-find*, list-map* etc are wrappers accepting Lisp functions and lambdas

(list-find* 'oddp 
            '(0 2 11 5 7))

(list-find-index* 'oddp 
                  '(0 2 11 4 5))

(list-filter* (lambda (x) (or (evenp x) (< 4 x))) 
              '(1 2 3 4 5))

(list-map* (lambda (x) (* 10 x))
           '(1 2 3 4 5))

(defgeneric find (predicate structure))
(defgeneric find-index (predicate structure))
(defgeneric filter (predicate structure))
(defgeneric map (function structure))
(defgeneric join (structure))
(defgeneric join-map (function structure))

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


(find 'evenp '(1 2 3 4))
(find-index 'evenp '(1 2 3 4))
(filter 'evenp '(1 2 3 4))
(map (lambda (x) (+ 100 x)) '(1 2 3 4))
(join 
 (list-cons 
  (list-single 0) 
  (list-single (list-cons 1 (list-single 2)))))


; ---------------------------------------------------------------------------------------------------

; Doremir.Set

(setf x (set-empty))
(setf x (set-single 1))
(setf x (set-dadd 1 x))             ; Set.add does not overwrite equals
(setf x (set-dremove 1 x))          ; Set.set does (for numbers, it doesn't matter)
(setf x (set-dadd (random 20) x))
(setf x (set-dremove (random 20) x))
(setf y (set-copy x))
(destroy x)

(set-size x)
(set-is-empty x)
(set-is-single x)
(set-has 1 x)
(set-get 1 x)                     

(set-is-subset-of y x)
(set-is-proper-subset-of y x)

(set-sum x y)
(set-intersection x y)
(set-product x y)
(set-difference x y)

(set-to-list x)
(import-list (set-to-list x))

; ---------------------------------------------------------------------------------------------------

; Doremir.Map

(setf x (map-empty))
(setf x (map-dadd "name" "hans" x))
(setf x (map-dadd "name" "sven" x))   ; Map.add does not overwrite equals
(setf x (map-dset "name" "sven" x))   ; Map.set does
(setf x (map-dadd "skills"
                  (list-single 1) x))
(setf x (map-add-entry 
         (pair-create "surname" 
                      "h�glund") x))
(setf x (map-remove "name" x))        ; Map.remove removes if present, otherwise does nothing
(setf x (map-remove "skills" x))
(setf y (map-copy x))
(destroy x)

(map-size x)
(map-is-empty x)
(map-is-single x)

(map-has-key "name" x)
(map-has-key "skills" x)
(from-pointer 'string (map-get "name" x))
(from-pointer 'list (map-get "skills" x))

(map-is-submap-of x y)
(map-is-proper-submap-of x y)

(map-to-list x)
(mapcar (lambda (x) (from-pointer 'pair x)) 
        (import-list (map-to-list x)))


; ---------------------------------------------------------------------------------------------------

; Doremir.Buffer

(setf x (buffer-create 10))      ; Always initialized to 0
(setf x (buffer-resize 20 x))    ; Will not lose data when increasing size
(setf x (buffer-create 1024))    ; Size and offset is given in size_t, i.e. bytes
(setf x (buffer-resize 2048 x))
(destroy x)


(defmethod get ((b buffer) i)
  (buffer-get x i))
(defmethod (setf get) (v (b buffer) i)
  (buffer-set x i v))


(cl:print x)
(get x 0)
(setf (get x 0) #xff)


; Size and access in bytes
(buffer-size x)
(buffer-get x 1)
(buffer-set x 1 10)

; Other types
(buffer-get-int16 x 1)
(buffer-set-int16 x 1 10)
(buffer-get-int32 x 1)
(buffer-set-int32 x 1 10)
(buffer-get-int64 x 1)
(buffer-set-int64 x 1 10)
(buffer-get-float x 1)
(buffer-set-float x 1 0.5)
(buffer-get-double x 1)
(buffer-set-double x 1 0.5d0)

(dotimes (i (buffer-size x))
  (buffer-set x i (mod i 256)))
(dotimes (i (buffer-size x))
  (buffer-set x i 0))

(cl:print x)

; Reading and writing raw buffers


; Reading and writing audio files

(setf x (buffer-read-audio "/Users/hans/Desktop/test.wav"))
(setf x (buffer-read-audio "/Users/hans/Desktop/Passager.wav"))
(setf x (from-pointer 'buffer (pair-snd x)))

(defun buffer-read-audio* (path)
  (let ((res (buffer-read-audio path)))
    (when (error-check res)
      (error-log nil (to-error res))
      (cl:error (error-message (to-error res))))
    res))

; Safe versions
(buffer-read-audio* "/Users/hans/Desktop/test.wav")
(buffer-read-audio* "does-not-exist.wav")

; ---------------------------------------------------------------------------------------------------

; Doremir.Midi

(setf x (midi-create-simple #x9 60 127))
(setf x (midi-create-sysex (buffer-create 1024)))
(setf y (midi-copy x))
(destroy x)

(midi-is-simple x)
(midi-status x)                   ; unsafe, assure is-simple
(midi-channel x)                  ; unsafe, assure is-simple
(midi-simple-data x)              ; unsafe, assure is-simple
(pair-fst (midi-simple-data x))   ; unsafe, assure is-simple
(pair-snd (midi-simple-data x))   ; unsafe, assure is-simple

(midi-is-sysex x)
(midi-sysex-data x)               ; unsafe, assure is-sysex

; TODO auto-convert expressions like this
(:note-on 60 127)
(:note-off 60 127)
(:control :volume 128)

; ---------------------------------------------------------------------------------------------------

; Doremir.Time

(time :minutes 1 :seconds 1/2)
(greater-than (time :minutes 1) (time :seconds 59))
(greater-than (time :minutes 1) (time :seconds 61))

(setf x (time :days 7))
(setf x (time :hours 1 :minutes 1 :seconds 2))
(setf x (time :seconds 3662))
(setf x (time :minutes 4 :seconds 1/5))
(setf x (time :minutes 4 :seconds 33.5))
(setf x (time :milliseconds 5512))
(setf x (time :nanoseconds 1341405))
(setf y (time-copy x))
(setf x (from-pointer 'time (add x y)))
(destroy x)

(time-days x)
(time-hours x)
(time-minutes x)
(time-seconds x)
(time-divisions x)

(time-to-iso x)
(from-pointer 'time (add x y))


; Clocks
(setf z (time-get-system-prec-clock))
(setf x (time-time z))
(setf x (time-ticks z))
(setf x (time-tick-rate z))



; ---------------------------------------------------------------------------------------------------
; Audio processing
; ---------------------------------------------------------------------------------------------------

; Doremir.Type

; Type expressions, auto-converted to type
(setf x nil)
(setf x :unit)
(setf x :i8)
(setf x :i16)
(setf x :i32)
(setf x :i64)
(setf x :f32)
(setf x :f64)
(setf x :ptr)
(setf x '(:pair :f32 :i32))
(setf x '(:f32 . :f32))
(setf x '((:f32 . :f32) . (:f32 . :f32)))
(setf x '(:frame :f32))
(setf x '(:vector (:pair :i8 :i32) 24))
(setf x '(:vector :f32 1024))

; We can also force conversion
(setf x (to-type :unit))
(setf x (to-type '(:pair :i8 :i8)))
(setf x (to-type '(:vector (:pair :i8 :i32) 24)))

(type-is-simple x)
(type-is-pair x)
(type-is-vector x)
(type-is-frame x)

(type-size-of 256 x)
(type-offset-of 256 x)
(type-align-of x)

; Multichannel
(setf x (type-repeat 8 :f32)) ; (f32,...)
(setf x (type-repeat 1 :f32)) ; f32
(setf x (type-repeat 0 :f32)) ; ()
(type-channels x)


; ---------------------------------------------------------------------------------------------------

(defcallback add-i8 :char ((c ptr) (x :char))
  (declare (ignore c))
  (+ x 1))

(defcallback add-f32 :float ((c ptr) (x :float))
  (declare (ignore c))
  (+ x 1))

(defcallback add-i8-i8 :char ((c ptr) (x :float) (y :float))
  (declare (ignore c))
  (+ x y))

(setf x (unary :i8 :i8 (callback add-i8) nil))
(setf x (unary :f32 :f32 (callback add-f32) nil))
(setf x (binary :i8 :i8 :i8 (callback add-i8-i8) nil))
(setf y x)

; ---------------------------------------------------------------------------------------------------

; Doremir.Processor

; TODO combine defcallback/unary/binary into single macro
; i.e. define-processor

;(type-offset-of 256 (input-type x))

(input-type x)
(output-type x)

(setf y (identity :i8))
(setf x (constant :i8 :i8 0))
(setf x (sequence x y))
(setf x (parallel x y))
(setf x (loop x))
(setf x (split :f32))
; TODO (setf x (delay :f32 44100))

(equal
  (input-type (parallel (identity :i8) (identity :i8)))
  (output-type (split :i8)))

(parallel
  (sequence (identity '(:frame :i8)) (constant '(:frame :i8) '(:frame :i16) nil) (identity '(:frame :i16)))
  (identity '(:vector :f32 1024))
  (identity '(:frame :f32)))

(sequence
 (split :i8)
 (parallel (identity :i8) (identity :i8))
 (identity '(:i8 . :i8))
 (binary :i8 :i8 :f32 (callback add-i8-i8) nil))

; TODO short names
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



; ---------------------------------------------------------------------------------------------------
; Event scheduling
; ---------------------------------------------------------------------------------------------------

; Doremir.Event

(setf x (event-never))
(setf x (event-now 12))
(setf x (event-delay (seconds 30) (event-now 13)))
(setf x (event-later (seconds 1/3) 48))
(setf x (event-merge (event-now 48) (event-later (seconds 1) 60)))

(run-event x)

(defun to-midi (x)
  (midi-create-simple #x90 (round (* 128 (/ x 1800))) 120))

(defun get-mouse-x-int (v)
  (round (pair-fst (from-pointer 'pair v))))


(setf mouse-down (system-event-mouse-down))
(setf mouse-down-x (event-map* 'get-mouse-x-int mouse-down))
(setf mouse-move (system-event-mouse-move))
(setf mouse-move-x (event-map* 'get-mouse-x-int mouse-move))
(setf key-down
 (event-map*
  (lambda (xs)
    (+ (- (list-index 1 (from-pointer 'list xs)) 97) 48))
  (system-event-key-down)))

(run-event key-down)
(run-event (input-slider))

(run-event
 (event-merge
  (event-send (to-receiver z) 0
              (event-map* (lambda (x)
                            (midi-create-simple
                             #x90 x 100))
                          key-down))
  (event-send (to-receiver z) 0
              (event-map* (lambda (x)
                            (midi-create-simple #xb0 7 (round (/ 100 (* 127 x)))))
                          (input-slider :title "Volume")))))


(defun run-event (event)
  (let* ((scheduler
          (scheduler-create
           (time-get-system-prec-clock))))
    (audioengine-log-info "Running event")
    (scheduler-schedule scheduler
                        (system-event-write-log event))
    (scheduler-loop-for (time :seconds 30) scheduler)
    ; later...
    (destroy scheduler)
    (audioengine-log-info "Stopped running event")))


; Event receiving values from an input slider
;
; This is the simplest way to create an event from an interface. Usually we want to
; use a single dispatcher and send values on separate channels.

(defun input-slider (&key (title "Input"))
  (let* ((chan 0)
         (disp (message-create-dispatcher))               ; Shared dispatcher
         (handler (lambda (interface value gesture)
                    (message-send
                        (to-receiver disp) chan value)))  ; The interface sends to disp/chan
         (class (capi:define-interface simple-slider ()
                  ()
                  (:panes
                   (slider capi:slider
                           :tick-frequency 10
                           :callback handler))))
         (interface (capi:display
                     (make-instance 'simple-slider
                                    :title title
                                    :best-width 500))))
    (event-receive (to-sender disp) chan)))               ; The event receives from disp/chan

(run-event (input-slider :title "Foo"))

(setf (capi:range-slug-start (slot-value i 'slider)) (random 100))



; ---------------------------------------------------------------------------------------------------
; Devices
; ---------------------------------------------------------------------------------------------------

; Doremir.Device.Audio

(setf s (device-audio-begin-session))
(error-check s)
(error-log nil (to-error s))
(error-message (to-error s))
(device-audio-end-session s)

(device-audio-all s)
(setf x (device-audio-default-input s))
(setf y (device-audio-default-output s))

(device-audio-name x)
(device-audio-host-name x)
(device-audio-has-input x)
(device-audio-has-output x)
(device-audio-input-type x)
(device-audio-output-type x)
(type-channels (device-audio-input-type x))
(type-channels (device-audio-output-type x))
(type-size-of 1024 (device-audio-input-type x))

(defcallback audio-status-changed ptr ((x ptr))
  (declare (ignore x))
  (capi:display-message "Audio setup changed")
  (audioengine-log-info "Audio setup changed"))
(device-audio-set-status-callback (callback audio-status-changed) nil s)

(setf p (processor-identity '(:pair (:frame :f32) (:frame :f32))))
(setf z (device-audio-open-stream x p y))
(device-audio-close-stream z)

; TODO unregister status callback?
; TODO status callback does not care about sessions (work even after session becomes inactive)


; ---------------------------------------------------------------------------------------------------

; Doremir.Device.Midi

(setf s (device-midi-begin-session))
(error-check s)
(error-log nil (to-error x))
(error-message (to-error s))
(device-midi-end-session s)

(list-length (device-midi-all s))
(setf x (from-pointer 'device-midi (nth 6 (import-list (device-midi-all s)))))
(setf x (device-midi-default-input s))
(setf y (device-midi-default-output s))

(device-midi-name x)
(device-midi-host-name x)
(device-midi-has-input x)
(device-midi-has-output x)

; FIXME (device-midi-set-status-callback (callback midi-status-changed) nil s)

(setf z (device-midi-open-stream x))
(device-midi-close-stream z) 

(message-send 
 (to-receiver z) 0 
 (midi-create-simple #x90 (+ 48 (random 12)) 120))



; ---------------------------------------------------------------------------------------------------

; Doremir.Device.File

(device-file-open)
(device-file-close)
(device-file-run)

; ---------------------------------------------------------------------------------------------------

; Doremir.Device.Buffer

(device-buffer-open)
(device-buffer-close)
(device-buffer-run)




; ---------------------------------------------------------------------------------------------------
; Utility
; ---------------------------------------------------------------------------------------------------

; Doremir.Plot

(setf b (buffer-create 1024))
(dotimes (i (/ (buffer-size b) 8))
  (let* ((i x)
         (y 0.0))
    (buffer-set-double b 0 0.5d1)))

(plot-buffer-double x nil nil)
(plot-buffer-float x nil nil)

; ---------------------------------------------------------------------------------------------------

; Doremir.Message

(setf x (message-create-dispatcher))
(setf x (message-create-lockfree-dispatcher))
(setf s (to-sender x))
(setf r (to-receiver x))

(message-send r 0 (random 20))
(message-send r 0 (123/456))
(message-send r 0 (to-list '(1 2 3)))

(progn
  (message-sync s)
  (dolist (x (from-list (message-receive s 0)))
    (cl:print (from-pointer 'list x))))

; ---------------------------------------------------------------------------------------------------

; Doremir.Atomic

(setf x (atomic-create))
(setf y (atomic-copy x))
(destroy x)

(atomic-exchange x 0 1)             ; FIXME does not work from Lisp
(atomic-exchange x 1 0)
(atomic-get x)
(atomic-set x 0)
(atomic-add x 1)
; (atomic-modify (lambda (x) x) x)

; ---------------------------------------------------------------------------------------------------

; Doremir.Atomic.Queue

(setf x (atomic-queue-create))
(destroy x)
(atomic-queue-write x (random 20))
(atomic-queue-read x)

; ---------------------------------------------------------------------------------------------------

; Doremir.Atomic.Stack

(setf x (atomic-stack-create))
(destroy x)
(atomic-stack-write x (random 20))
(atomic-stack-read x)


; ---------------------------------------------------------------------------------------------------

; Doremir.Thread
;
; You can use these or the thread system of your Lisp implementation (i.e. MP in LispWorks, 
; native threads in SBCL). 
;
; You can use AE mutex to syncronize threads created by Lisp and vice versa.


(defcallback thread-action ptr ((data ptr))
  (declare (ignore data))
  (audioengine-log-info "Started thread.")
  (thread-holding (y) 
                  (thread-sleep 3000))
  (audioengine-log-info "Done."))


(setf x (thread-create (callback thread-action) nil))
(thread-main)
(thread-current)
(equal (thread-current) (thread-main))
(thread-sleep 3000)
(thread-join x)
(thread-detach x)

(setf y (thread-create-mutex))
(destroy y)
(thread-lock y)
(thread-try-lock y)
(thread-unlock y)
(thread-holding 
 (y)
 (audioengine-log-info "I have the mutex"))








