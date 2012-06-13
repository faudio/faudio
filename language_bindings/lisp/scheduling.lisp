;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass future (object)
  ()
  (:documentation
"A future represents a computation that have been scheduled for execution
in an audio stream. All scheduling functions return future objects, which
may be used to interrupt the scheduled action.

Time may be measured in samples or milliseconds. To convert between the two,
see samples-to-milliseconds and milliseconds-to-samples.

Futures are scheduled using the `do`, `send` and `receive` family of functions.

See [Future](@ref doremir::scl::Future)."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass schedule-options (object)
  ((unit        :type :keyword)
   (groups      :type :list)
   (repeats     :type :integer)
   (interval    :type :integer))
   (:documentation
"Defines options that can be passed to all scheduling functions.       

  unit      Either :samples or :milliseconds. Default is :milliseconds
  groups    A list of future-groups to which this future will be added.
  repeats   Number of times this future will be repeated (default 1).
  interval  Time to wait between repetitions (default same as time).
"))

(defun default-schedule-options ()
  (native-call scl-default-schedule-options
    :schedule-options ()))

(defmethod unit ((obj schedule-options))
  (native-call scl-schedule-options-get-unit
    (:enum :samples :milliseconds)
    ((obj :schedule-options))))

(defmethod groups ((obj schedule-options))
  (native-call scl-schedule-options-get-groups
    (:list :future-group)
    ((obj :schedule-options))))

(defmethod repeats ((obj schedule-options))
  (native-call scl-schedule-options-get-repeats
    :integer
    ((obj :schedule-options))))

(defmethod interval ((obj schedule-options))
  (native-call scl-schedule-options-get-interval
    :integer
    ((obj :schedule-options))))

(defmethod (setf unit) (unit (obj schedule-options))
  (native-call scl-schedule-options-set-unit :void
    ((obj :schedule-options) (unit (:enum :samples :milliseconds)))) unit)

(defmethod (setf groups) (groups (obj schedule-options))
  (native-call scl-schedule-options-set-groups :void
    ((obj :schedule-options)
     (groups (:list :future-group)))) groups)

(defmethod (setf repeats) (repeats (obj schedule-options))
  (native-call scl-schedule-options-set-repeats :void
    ((obj :schedule-options)
     (repeats :integer))) repeats)

(defmethod (setf interval) (interval (obj schedule-options))
  (native-call scl-schedule-options-set-interval :void
    ((obj :schedule-options)
     (interval :integer))) interval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass send-options (schedule-options)
  ((kind        :type :keyword)
   (processors  :type :list)
   (devices     :type :list)
   (channels    :type :list))
   (:documentation
"
Defines options that can be passed to the send family of functions.
  kind      Either :midi or :audio.
"))

(defun default-send-options ()
  (native-call scl-default-send-options :send-options ()))

(defmethod kind ((obj send-options))
  (native-call scl-send-options-get-kind (:enum :audio :midi)
    ((obj :send-options))))

(defmethod processors ((obj send-options))
  (native-call scl-send-options-get-processors (:list :audio-processor)
    ((obj :send-options))))

(defmethod devices ((obj send-options))
  (native-call scl-send-options-get-devices (:list :midi-device)
    ((obj :send-options))))

(defmethod channels ((obj send-options))
  (native-call scl-send-options-get-channels (:list :integer)
    ((obj :send-options))))

(defmethod (setf kind) (kind (obj send-options))
  (native-call scl-send-options-set-kind :void
    ((obj :send-options) (kind (:enum :audio :midi)))) kind)

(defmethod (setf processors) (processors (obj send-options))
  (native-call scl-send-options-set-processors :void
    ((obj :send-options) (processors (:list :audio-processor)))) processors)

(defmethod (setf devices) (devices (obj send-options))
  (native-call scl-send-options-set-devices :void
    ((obj :send-options) (devices (:list :midi-device)))) devices)

(defmethod (setf channels) (channels (obj send-options))
  (native-call scl-send-options-set-channels :void
    ((obj :send-options) (channels (:list :integer)))) channels)

;; TODO: check if the slots are copied deep enough (maybe copy-list or copy-tree is needed for some slots)
(defmethod copy ((self send-options))
  (let ((new (default-send-options)))
    (setf (unit new) (unit self)
          (groups new) (groups self)
          (repeats new) (repeats self)
          (interval new) (interval self)
          (kind new) (kind self)
          (processors new) (processors self)
          (devices new) (devices self)
          (channels new) (copy-list (channels self)))
    new))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass receive-options (schedule-options)
  ((kind        :type :keyword)
   (processors  :type :list)
   (devices     :type :list)
   (channels    :type :list)))

(defun default-receive-options ()
  (native-call scl-default-receive-options :receive-options ()))

(defmethod kind ((obj receive-options))
  (native-call scl-receive-options-get-kind (:enum :audio :midi)
    ((obj :receive-options))))

(defmethod processors ((obj receive-options))
  (native-call scl-receive-options-get-processors (:list :audio-processor)
    ((obj :receive-options))))

(defmethod devices ((obj receive-options))
  (native-call scl-receive-options-get-devices (:list :midi-device)
    ((obj :receive-options))))

(defmethod channels ((obj receive-options))
  (native-call scl-receive-options-get-channels (:list :integer)
    ((obj :receive-options))))


(defmethod (setf kind) (kind (obj receive-options))
  (native-call scl-receive-options-set-kind :void
    ((obj :receive-options) (kind (:enum :audio :midi)))) kind)

(defmethod (setf processors) (processors (obj receive-options))
  (native-call scl-receive-options-set-processors :void
    ((obj :receive-options) (processors (:list :audio-processor)))) processors)

(defmethod (setf devices) (devices (obj receive-options))
  (native-call scl-receive-options-set-devices :void
    ((obj :receive-options) (devices (:list :midi-device)))) devices)

(defmethod (setf channels) (channels (obj receive-options))
  (native-call scl-receive-options-set-channels :void
    ((obj :receive-options) (channels (:list :integer)))) channels)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-action (name (time) &body body)
  "Defines an schedulable action.
Each action must have the form (defaction my-action (time) ...)"
  `(fli:define-foreign-callable
       (,name :result-type :void
              :calling-convention :cdecl)
       ((,time :int))
     (declare (ignorable ,time))
     (mp:last-callback-on-thread)
     ,@body))


(defmethod do-now ((obj stream) action
                   &optional schedule-options)
"Executes the given action as soon as possible. 
Returns a future object."
  (native-call scl-schedule-now :future
    ((obj :stream)
     ((fli:make-pointer :symbol-name action) (:function :dummy))
     (schedule-options :object))
    :errors (stream-error)))


(defmethod do-later ((obj stream) time action
                   &optional schedule-options)
"Executes the given action after the given amount of time. 
Returns a future object."
  (native-call scl-schedule-later :future
    ((obj :stream)
     (time :integer)
     ((fli:make-pointer :symbol-name action) (:function :dummy))
     (schedule-options :object))
    :errors (stream-error)))


(defmethod do-at ((obj stream) time action
                   &optional schedule-options)
"Executes the given action at the given point in time. 
Returns a future object."
  (native-call scl-schedule-at :future
    ((obj :stream)
     (time :integer)
     ((fli:make-pointer :symbol-name action) (:function :dummy))
     (schedule-options :object))
    :errors (stream-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod send-now ((obj stream) message
                     &optional send-options)
"Sends the given message as soon as possible.
Returns a future object.
May signal stream-error."
  (native-call
    scl-send-now :future
    ((obj :stream)
     (message (:list :atom))
     (send-options :object))
    :errors (stream-error)))


(defmethod send-later ((obj stream) time message
                       &optional send-options)
"Sends the given message after the given amount of time.
Returns a future object.
May signal stream-error."
  (native-call
    scl-send-later :future
    ((obj :stream)
     (time :integer)
     (message (:list :atom))
     (send-options :object))
    :errors (stream-error)))


(defmethod send-at ((obj stream) time message
                     &optional send-options)
"Sends the given messages at the given point in time.
Returns a future object.
May signal stream-error."
  (native-call
    scl-send-at :future
    ((obj :stream)
     (time :integer)
     (message (:list :atom))
     (send-options :object))
    :errors (stream-error)))


(defun send-note-now (&key stream channel note-number duration velocity send-options)
  (list (send-now stream (list (+ 144 channel) note-number velocity) send-options)
        (send-later stream duration (list (+ 144 channel) note-number 0) send-options)))

(defun send-note-later (&key stream time channel note-number duration velocity send-options)
  (list (send-later stream time (list (+ 144 channel) note-number velocity) send-options)
        (send-later stream (+ time duration) (list (+ 144 channel) note-number 0) send-options)))

(defun send-note-at (&key stream time channel note-number duration velocity send-options)
  (list (send-at stream time (list (+ 144 channel) note-number velocity) send-options)
        (send-at stream (+ time duration) (list (+ 144 channel) note-number 0) send-options)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-receiver (name (time msg) &body body)
"Defines a message receiver.
Each receiver must have the form (define-receiver my-receiver (time msg) ...)"
  `(fli:define-foreign-callable
    (,name :result-type :void
           :calling-convention :cdecl)
    ((,time :int)
     (_array2 (:pointer (:pointer :void)))
     (_length :int))
  (let ((,msg ,(import-list-2 :atom '_length '_array2)))
    (mp:last-callback-on-thread)
    ,@body)))

(defmethod receive ((obj stream) receiver
                     &optional receive-options)
"Start passing incoming messages to the given receiver.
Returns a future object."
  (native-call scl-receive :future
    ((obj :stream)
     ((fli:make-pointer :symbol-name receiver) (:function :dummy))
     (receive-options :object))
    :errors (stream-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass future-group (object)
  ((interruption-mode :type keyword))
   (:documentation
"Provides a way to interrupt a group futures atomically.

Each future group is associated with an interruption-mode, which specifies how
it handles late interruptions. A late interruption is an interruption that
occurs when some but not all members of a future group have been fired.

The interruption-modes prescribes the following actions:

* `:simple` Interrupt all remaining futures
* `:transactional` Execute remaining futures on scheduled time
                    
See [FutureGroup](@ref doremir::scl::FutureGroup)."))


(defun make-future-group (&optional (mode :simple))
  (native-call scl-new-future-group 
    :future-group 
    ((mode (:enum :simple :forcing :transactional)))))

(defmethod interruption-mode ((obj future-group))
  (native-call scl-future-group-interruption-mode 
    (:enum :simple :forcing :transactional) 
    ((obj :future-group))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric interrupt (object)
   (:documentation
"Interrupts this object, preventing it from being fired. If it has already been
fired, this function has no effect.

Note that a future might have been fired concurrently by another thread, event if
its actions has not yet been observed by the caller thread."))

(defmethod interrupt ((f future))
  (native-call scl-interrupt-future :void ((f :future))))

(defmethod interrupt ((g future-group))
  (native-call scl-interrupt-future-group :void ((g :future-group))))

