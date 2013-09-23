

#|
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#


#|
    ## Conventions

        All Modulo/C functions are exposed directly. Name is the same as in C except that:

            - There is no global 'ae' prefix, instead all symbols are in the 'ae' package.
            - Names are separated by dashes instead of underscores.

        For example 'ae_device_audio_name' becomes (ae:device-audio-name x).
        
        Sometimes a Lisp wrapper is added to adapt error checking, wrapping of closures etc. The name
        is the same as the wrapped function with a star suffix. For example, (list-find) is
        available in two forms:
    
          list-find     accepts CFFI callbacks only
          list-find*    accepts functions

        Note that the Lisp wrappers are a work in progress. See utility.lisp for more information.
        
    ## Types

        Opaque types (most of them) are represented by a Lisp class of the same name. For example
        the type String in module Doremir.String is represented by a type (defclass string).

    ## Resource management
        
        This is manual. Each created value object must be destroyed by passing it to (destroy), or
        another destructive function such as (list-dcons). See the reference page 'Data Structures'
        for more information.

        We might want to hook into the Lisp GC using something like the trivial garbage; however
        in many cases this might not necessarilty be practical. Things like sessions, devices and 
        buffers often needs to be managed manually in any case (the same problem applies to streams
        and sockets).
                                                                  
    ## Undefined behaviour

      - Calling a function on a destroyed object is undefined
      - Calling (from-pointer) with the wrong type is undefined
      - Calling a generic function on a type that does not implement a required interface is undefined
|#

(in-package :faudio)

(defvar x nil)
(defvar y nil)
(defvar z nil)
(defvar s nil)
(defvar d nil)
(defvar p nil)

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
(setf x "hello")
(setf y "holla")
(setf x 77/88)
(setf y 33/44)
(setf x (export-list '()))
(setf y (export-list '(1 2 3)))

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
(string-from-json "[1,2,3]")
(string-from-json "{\"foo\":null, \"bar\":[1,2,3]}")

; ---------------------------------------------------------------------------------------------------
;
; Doremir.AudioEngine

(audioengine-initialize)
(audioengine-terminate)
(audioengine-set-log-file "/Users/hans/Library/Logs/FAE.log")
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
;
; Note:
;   Modulo is not smart enough to translate enums to Lisp yet
;   See the module file for the definition

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
(dynamic-get-type 1/2)


; ---------------------------------------------------------------------------------------------------
; Data structures
; ---------------------------------------------------------------------------------------------------
;
; Doremir.Ratio
;
; AE ratios are interchangable with Lisp rationals.

(setf x (ratio-create 1 2))
(setf y (ratio-create 278 12))
(ratio-destroy x)

(ratio-num x)
(ratio-denom x)

(ratio-add x y)
(ratio-subtract x y)
(ratio-multiply x y)
(ratio-divide x y)
(ratio-succ x)
(ratio-pred x)
(ratio-negate x)
(ratio-recip x)

(ratio-create 1 2)
(ratio-succ (/ 1 2))
(ratio-recip (/ 567 235))

; ---------------------------------------------------------------------------------------------------
;
; Doremir.String
;
; AE strings are interchangable with Lisp strings.
;
; Note that LispWorks does not support the full Unicode range (only UCS-2).

(setf x (string-empty))
(setf x (string-single 104))
(setf x (string-repeat 30 (char-int #\a)))
(setf x (string-append "hans " "höglund"))
(destroy x)

(string-length "högtalare")
(code-char (string-char-at 0 "foo"))

(string-matches "a*b*c+" "aaacc")


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

; Conversion
(import-pair (pair-create 1 2)))
(export-pair '(1 . 2))

; ---------------------------------------------------------------------------------------------------

; Doremir.List

; AE lists are distinct from Lisp lists:
;
;   * AE lists print as [1,2,3], not as '(1 2 3).
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

(list-find* 'evenp '(1 2 3 4))
(list-find-index* 'evenp '(1 2 3 4))
(list-filter* 'evenp '(1 2 3 4))
(list-map* (lambda (x) (+ 100 x)) '(1 2 3 4))

(find 'evenp '(1 2 3 4))
(find-index 'evenp '(1 2 3 4))
(filter 'evenp '(1 2 3 4))
(map (lambda (x) (+ 100 x)) '(1 2 3 4))
(join
 (list-cons
  (list-single 0)
  (list-single (list-cons 1 (list-single 2)))))


; Mixing AE and Lisp lists
(list-append '(1 2 3) (list-single 4))
(list-dcons 1 '())
(list-is-empty '())
(list-is-single '(1))

(export-list '(1 2 3))
(import-list (export-list '(1 2 3)))


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

(set-to-list x)                   ; Convert to AE list
(import-list (set-to-list x))     ; Convert to Lisp list

; ---------------------------------------------------------------------------------------------------

; Doremir.Map

(setf x (map-empty))
(setf x (map-dadd "name" "hans" x))
(setf x (map-dadd "name" "sven" x))   ; Map.add does not overwrite equals
(setf x (map-dset "name" "sven" x))   ; Map.set does
(setf x (map-dset "skills"
                  (list-single 1) x))
(setf x (map-add-entry
         (pair-create "surname"
                      "höglund") x))
(setf x (map-remove "name" x))        ; Map.remove removes if present, otherwise does nothing
(setf x (map-remove "skills" x))
(setf y (map-copy x))
(destroy x)

(map-size x)
(map-is-empty x)
(map-is-single x)

(map-has-key "name" x)
(map-has-key "skills" x)
(map-get "name" x)
(map-get "skills" x)

(map-is-submap-of x y)
(map-is-proper-submap-of x y)

(map-to-list x)
(mapcar #'import-pair 
        (import-list 
         (map-to-list x)))


; ---------------------------------------------------------------------------------------------------

; Doremir.Buffer
;
; The AE buffers are simply raw blocks of memory with a known size.

; All memory is allocated outside the control of Lisp and must be freed by destroying 
; the buffer. You can acces individual elements using the (get) function, copy the buffer 
; to the Lisp heap using (buffer-to-array) and copy it back using (array-to-buffer).
;
; Reading or writing outside the range is undefined, but fails with an error message in
; a debug build of the AE.

(setf x (buffer-create 10))
(setf x (buffer-resize 20 x))
(setf x (buffer-create 1024))
(setf x (buffer-resize 2048 x))
(destroy x)

(cl:print x)

(size x)
(get x 0)
(setf (get x 0) #xff)

(dotimes (i (size x))
  (setf (get x i) (mod i 256)))
(dotimes (i (size x))
  (setf (get x i) 0))

; Typed get
; (buffer-get-int16 x 1)
; (buffer-set-int16 x 1 10)
; (buffer-get-int32 x 1)
; (buffer-set-int32 x 1 10)
; (buffer-get-int64 x 1)
; (buffer-set-int64 x 1 10)
(buffer-get-float x 1)
(buffer-set-float x 1 0.5)
(buffer-get-double x 1)
(buffer-set-double x 1 0.5d0)

; Conversion
(setf a (buffer-to-array x))
(setf x (array-to-buffer a))

; I/O
(setf x (buffer-read-audio* "/Users/hans/Desktop/test.wav"))
(setf x (buffer-read-audio* "does-not-exist.wav"))
(setf x 
      (from-pointer 'buffer (pair-second x)))

; ---------------------------------------------------------------------------------------------------

; Doremir.Midi

(setf x (midi #x91 60 127))
(setf x (midi-create-sysex (buffer-create 1024)))
(setf y (midi-copy x))
(destroy x)

(midi-is-simple x)
(midi-status x)
(midi-channel x)
(midi-simple-data x)
(pair-fst (midi-simple-data x))
(pair-snd (midi-simple-data x))

(midi-is-sysex x)
(midi-sysex-data x)


; ---------------------------------------------------------------------------------------------------

; Doremir.Time
;
; Use the (time) function to create a time value. All the given components are added, 
; together so (time :minutes 1 :seconds 2) is equivalent to (time :seconds 62).
;
; All components except seconds must be integers. Seconds may be rational or 
; real and are implicitly coerced to rationals. 

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
;(time-to-milliseconds x)
(time-to-seconds x)

; Clocks
(setf z (time-get-system-prec-clock))
(setf x (time-time z))
(setf x (time-ticks z))
(setf x (time-tick-rate z))



; ---------------------------------------------------------------------------------------------------
; Audio processing
; ---------------------------------------------------------------------------------------------------

(defmacro time
  (&rest args) `(signal-time ,@args))
(defmacro noise
  (&rest args) `(signal-random ,@args))
(defmacro constant
  (&rest args) `(signal-constant ,@args))
(defmacro input
  (&rest args) `(signal-input ,@args))
(defmacro output
  (&rest args) `(signal-output ,@args))
(defmacro delay
  (&rest args) `(signal-delay ,@args))
(defmacro +~
  (&rest args) `(signal-add ,@args))
(defmacro *~
  (&rest args) `(signal-multiply ,@args))
(defmacro sin~
  (&rest args) `(signal-sin ,@args))



; Signals

(setf x (time))
(setf x (noise))
(setf x (constant 0.5d0))
(setf x (input 1))

(setf x (*~ (constant 0.3d0) 
            (sin~ (signal-line 440d0))))

(signal-run-file (* 44100 10) x "/Users/hans/audio/out.wav")





; Doremir.Type
;
; You can always give a function expecting a type a *type expression* (see below). 
; Use (type *expr*) to force conversion.

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

(setf x (type nil))
(setf x (type :i32))
(setf x (type :unit))
(setf x (type '(:pair :i8 :i8)))
(setf x (type '(:vector (:pair :i8 :i32) 24)))
; etc

(type-is-simple x)
(type-is-pair x)
(type-is-vector x)
(type-is-frame x)

(type-size-of 256 x)
(type-offset-of 256 x)
(type-align-of x)

(setf x (type-repeat 8 :f32)) ; (f32,...)
(setf x (type-repeat 1 :f32)) ; f32
(setf x (type-repeat 0 :f32)) ; ()
(type-channels x)




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

(device-audio-set-status-callback* (lambda ()
  (capi:display-message "Audio setup changed")
  (audioengine-log-info "Audio setup changed")) s)

(setf p (processor-identity '(:pair (:frame :f32) (:frame :f32))))
(setf z (device-audio-open-stream x p y))
; close-all!
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

(device-midi-all s)
(setf x (from-pointer 'device-midi (nth 6 (import-list (device-midi-all s)))))
(setf x (device-midi-default-input s))
(setf y (device-midi-default-output s))

(device-midi-name x)
(device-midi-host-name x)
(device-midi-has-input x)
(device-midi-has-output x)

; FIXME (device-midi-set-status-callback (callback midi-status-changed) nil s)
(device-midi-set-status-callback* (lambda ()
  (capi:display-message "Audio setup changed")
  (audioengine-log-info "Audio setup changed")) s)

(defun register-midi-listener ()
  (device-midi-set-status-callback* (lambda ()
                                      (capi:display-message "Audio setup changed")
                                      (audioengine-log-info "Audio setup changed")) s)
  )
(mp:process-send mp:*main-process* #'register-midi-listener)




(setf z (device-midi-open-stream x))
(device-midi-close-stream z)

;(setf z (system-event-send-log))
(message-send
 (to-receiver z) 0
 (midi #x90 (+ 48 (random 12)) 120))



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

(message-send r 0 123)
(message-send r 0 (export-list '(1 2 3)))

(progn
  (message-sync s)
  (dolist (x (import-list (message-receive s 0)))
    (cl:print (from-dynamic x))))

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
; You can use an AE mutex to syncronize threads created by Lisp and vice versa.

(setf x (thread-create* (lambda () 
                          (audioengine-log-info 
                           "And there's someone in a thread."))))
(thread-join x)
(thread-detach x)

(thread-sleep 3000)

(thread-main)
(thread-current)

(defvar *my-thread* nil)
(cl:print *my-thread*)
(mp:process-send mp:*main-process*
  (setf *my-thread* (thread-current))
 )

(equal 
  (thread-current) (thread-main))


; Mutexes

(setf y (thread-create-mutex))
(destroy y)

(thread-lock y)
(thread-try-lock y)
(thread-unlock y)

(thread-holding
 (y)
 (audioengine-log-info "I have the mutex"))

(thread-create* (lambda ()
  (audioengine-log-info "I am waiting.")
  (thread-holding (y) 
                  (audioengine-log-info "Finally, got it!"))))



