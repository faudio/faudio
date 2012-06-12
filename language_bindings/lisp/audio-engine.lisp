;;;-*- Mode: Lisp; Package: (:audio) -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  audio-engine.lisp
;;
;;  Copyright (c) 2011 DoReMIR http://www.doremir.com
;;
;;  Author:  Hans Hoglund
;;  Created: 2011-10-19

(in-package :audio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass object ()
  ((native-object :initarg :native-object))
  (:documentation
"Root class for object in the audio system."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition audio-error ()
  ((native-object :initarg :native-object)
   (message :type string))
  (:documentation "An error in the audio system."))

(define-condition portmidi-error (audio-error)
  ((error-code :type integer))
  (:documentation "An error in portmidi."))

(define-condition portaudio-error (audio-error)
  ((error-code :type  integer))
  (:documentation "An error in portaudio."))

(define-condition stream-error (audio-error) ()
  (:documentation "An error related to a stream."))

(define-condition dsp-error (audio-error) ()
  (:documentation "An error related to an audio processor."))

(defmethod message ((obj audio-error))
"A string describing the error."
  (native-call scl-error-message :string ((obj :error))))

(defmethod error-code ((obj portmidi-error))
"Portmidi-specific error code"
  (native-call scl-portmidi-error-code :int ((obj :portmidi-error))))

(defmethod error-code ((obj portaudio-error))
"Portaudio-specific error code"
  (native-call scl-portaudio-error-code :int ((obj :portaudio-error))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass session (object)
   ()
   (:documentation
"A set of available devices.

This class abstracts over single state API such as Portaudio and Portmidi to 
provide multiple views of the devices available on the system while not compromising 
functionality of the APIs.

See [Session](@ref doremir::scl::Session)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass midi-device (object)
  ((name       :type string)
   (host-name  :type string)
   (has-input  :type boolean)
   (has-output :type boolean))
   (:documentation
"Represents a Midi device. 

See [MidiDevice](@ref doremir::scl::MidiDevice)."))

(defun midi-devices ()
"Returns a list of midi devices currently available.
May signal portmidi-error."
  (native-call scl-midi-devices (:list :midi-device)  ()
    :errors (portmidi-error)))

(defun default-midi-input-device ()
"Returns the current default midi input device or nil if there are no
input devices available.
May signal portmidi-error."
  (native-call scl-default-midi-input-device :midi-device ()
    :errors (portmidi-error)))

(defun default-midi-output-device ()
"Returns the current default midi output device or nil if there are no
output devices available.
May signal portmidi-error."
  (native-call scl-default-midi-output-device :midi-device ()
    :errors (portmidi-error)))

(defun midi-sources ()
"Returns a list of midi sources currently available.
May signal portmidi-error."
  (remove-if #'has-output (midi-devices)))

(defun midi-destinations ()
"Returns a list of midi sources currently available.
May signal portmidi-error."
  (remove-if #'has-input (midi-devices)))

(defmethod name ((obj midi-device))
  (native-call scl-midi-device-name :string ((obj :midi-device))))

(defmethod host-name ((obj midi-device))
  (native-call scl-midi-device-host-name :string ((obj :midi-device))))

(defmethod has-input ((obj midi-device))
  (native-call scl-midi-device-has-input :boolean ((obj :midi-device))))

(defmethod has-output ((obj midi-device))
  (native-call scl-midi-device-has-output :boolean ((obj :midi-device))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass audio-host (object)
  ((name              :type string)
   (devices           :type list))
   (:documentation
"Represents an audio host, i.e, CoreAudio, ASIO.

See [AudioHost](@ref doremir::scl::AudioHost)."))

(defmethod name ((obj audio-host))
  (native-call scl-audio-host-name :string ((obj :audio-host))))

(defmethod devices ((obj audio-host))
  (native-call scl-audio-host-devices (:list :audio-device) ((obj :audio-host))))


(defclass audio-device (object)
  ((name                :type string)
   (host                :type audio-host)
   (num-inputs          :type integer)
   (num-outputs         :type integer)
   (low-input-latency   :type double)
   (high-input-latency  :type double)
   (low-output-latency  :type double)
   (high-output-latency :type double)
   (sample-rate         :type integer))
   (:documentation
"Represents an audio device, i.e. a sound card or a virtual device.

See [AudioDevice](@ref doremir::scl::AudioDevice)."))

(defmethod name ((obj audio-device))
  (native-call scl-audio-device-name :string ((obj :audio-device))))

(defmethod host ((obj audio-device))
  (native-call scl-audio-device-host :audio-host ((obj :audio-device))))

(defmethod num-inputs ((obj audio-device))
  (native-call scl-audio-device-max-input-channels :integer ((obj :audio-device))))

(defmethod num-outputs ((obj audio-device))
  (native-call scl-audio-device-max-output-channels :integer ((obj :audio-device))))

(defmethod low-input-latency ((obj audio-device))
  (native-call scl-audio-device-default-low-input-latency :double ((obj :audio-device))))

(defmethod high-input-latency ((obj audio-device))
  (native-call scl-audio-device-default-high-input-latency :double ((obj :audio-device))))

(defmethod low-output-latency ((obj audio-device))
  (native-call scl-audio-device-default-low-output-latency :double ((obj :audio-device))))

(defmethod high-output-latency ((obj audio-device))
  (native-call scl-audio-device-default-high-output-latency :double ((obj :audio-device))))

(defmethod sample-rate ((obj audio-device))
  (native-call scl-audio-device-default-sample-rate :integer ((obj :audio-device))))


(defun audio-hosts ()
"Returns a list of audio hosts currently available.
May signal portaudio-error."
  (native-call scl-audio-hosts (:list :audio-host) ()
   :errors (portaudio-error)))

(defun default-audio-host ()
"Returns the current default audio host.
May signal portaudio-error."
  (native-call scl-default-audio-host :audio-host ()
   :errors (portaudio-error)))

(defun default-audio-input-device ()
"Returns the current default audio input device.
May signal portaudio-error."
  (native-call scl-default-audio-input-device :audio-device ()
   :errors (portaudio-error)))

(defun default-audio-output-device ()
"Returns the current default audio output device.
May signal portaudio-error."
  (native-call scl-default-audio-output-device :audio-device ()
   :errors (portaudio-error)))

(defun audio-sources ()
"Returns a list of audio sources currently available.
May signal portaudio-error."
  (let ((sources nil))
    (dolist (h (audio-hosts))
      (dolist (d (devices h))
        (when (plusp (num-inputs d))
          (push d sources))))
    sources))

(defun audio-destinations ()
"Returns a list of audio destinations currently available.
May signal portaudio-error."
  (let ((destinations nil))
    (dolist (h (audio-hosts))
      (dolist (d (devices h))
        (when (plusp (num-outputs d))
          (push d destinations))))
    destinations))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass audio-processor (object)
  ((controls    :type list)
   (num-inputs  :type integer)
   (num-outputs :type integer))
   (:documentation
"An object that transforms audio signals. May be atomic or created by combinators.

See [AudioProcessor](@ref doremir::scl::AudioProcessor)."))

(defclass fluidsynth-processor (audio-processor)
  ()
  (:documentation
"See [FluidSynth](@ref doremir::scl::FluidSynth)."))

(defclass au-processor (audio-processor)
  ()
  (:documentation
"See [AudioUnitProcessor](@ref doremir::scl::AudioUnitProcessor)."))

(defclass vst-processor (audio-processor)
  ()
  (:documentation
"See [VstProcessor](@ref doremir::scl::VstProcessor)."))


(defun load-fluidsynth (soundfont-path)
"Creates a FluidSynth instance using the given sound font.
May signal dsp-error."
  (native-call scl-load-fluidsynth :audio-processor
    ((soundfont-path :string)) :errors (dsp-error)))

(defun load-plugins (&optional plugin-path)
"Load the given set of AU or VST plug-ins."
  nil)

(defun sequence (&rest processors)
"Creates a sequential processor from the given processors.
The output from the first processor is fed into the input of the second and so
on. The number of output and input channels at each such connection point must
match exactly, or a dsp-error is signaled."
  (native-call scl-sequence :audio-processor
    ((processors (:list :audio-processor)))
    :errors (dsp-error)))

(defun parallel (&rest processors)
"Creates a parallel processor from the given processors.
The resulting processor has x inputs and y outputs, where x is the total
number of inputs channels in the given list of processors and y is the total
number of outputs."
  (native-call scl-parallel :audio-processor
    ((processors (:list :audio-processor)))
    :errors (portaudio-error)))

(defmethod controls ((obj audio-processor))
  (native-call scl-processor-controls (:list :string) ((obj :audio-processor))))

(defmethod num-inputs ((obj audio-processor))
  (native-call scl-processor-num-inputs :integer ((obj :audio-processor))))

(defmethod num-outputs ((obj audio-processor))
  (native-call scl-processor-num-outputs :integer ((obj :audio-processor))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass device-stream-options (object)
  ((sample-rate)
   (audio-buffer-size)
   (audio-latency)
   (midi-latency)
   (non-blocking)
   (exclusive-mode)))

(defun default-device-stream-options ()
  (native-call scl-default-device-stream-options
    :device-stream-options ()))

(defmethod sample-rate ((obj device-stream-options))
  (native-call scl-device-stream-options-get-sample-rate :integer
    ((obj :device-stream-options))))

(defmethod (setf sample-rate) (value (obj device-stream-options))
  (native-call scl-device-stream-options-set-sample-rate :void
    ((obj :device-stream-options) (value :integer))) value)

(defmethod audio-buffer-size ((obj device-stream-options))
  (native-call scl-device-stream-options-get-audio-buffer-size :integer
    ((obj :device-stream-options))))

(defmethod (setf audio-buffer-size) (value (obj device-stream-options))
  (native-call scl-device-stream-options-set-audio-buffer-size :void
    ((obj :device-stream-options) (value :integer))) value)

(defmethod audio-latency ((obj device-stream-options))
  (native-call scl-device-stream-options-get-audio-latency :double
    ((obj :device-stream-options))))

(defmethod (setf audio-latency) (value (obj device-stream-options))
  (native-call scl-device-stream-options-set-audio-latency :void
    ((obj :device-stream-options) (value :double))) value)

(defmethod midi-latency ((obj device-stream-options))
  (native-call scl-device-stream-options-get-midi-latency :double
    ((obj :device-stream-options))))

(defmethod (setf midi-latency) (value (obj device-stream-options))
  (native-call scl-device-stream-options-set-midi-latency :void
    ((obj :device-stream-options) (value :double))) value)

(defmethod non-blocking ((obj device-stream-options))
  (native-call scl-device-stream-options-is-non-blocking :boolean
    ((obj :device-stream-options))))

(defmethod (setf non-blocking) (value (obj device-stream-options))
  (native-call scl-device-stream-options-set-non-blocking :void
    ((obj :device-stream-options) (value :boolean))) value)

(defmethod exclusive-mode ((obj device-stream-options))
  (native-call scl-device-stream-options-is-exclusive-mode :boolean
    ((obj :device-stream-options))))

(defmethod (setf non-blocking) (value (obj device-stream-options))
  (native-call scl-device-stream-options-set-exclusive-mode :void
    ((obj :device-stream-options) (value :boolean))) value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-handler (name (time error) &body body)
  `(fli:define-foreign-callable
    (,name :result-type :void
           :calling-convention :cdecl)
    ((,time :int)
     (_pointer (:pointer :void)))
  (mp:last-callback-on-thread)
  (let ((,error (convert-object :audio-error _pointer)))
    ,@body)))
    

(defclass stream (object)
  ((type              :type keyword)
   (sample-rate       :type integer)
   (audio-buffer-size :type integer)
   (running           :type boolean))
   (:documentation
"A synchronous audio or midi computation.

See [Stream](@ref doremir::scl::Stream)."))


(defun open-device-stream (&key (midi-input nil)
                                (midi-output nil)
                                (audio-input nil)
                                (audio-output nil)
                                (audio-processor nil)
                                (options nil))
"Opens a real-time audio stream on the given devices.

If no processor or audio devices are provided, a midi-only stream is returned.
If such a stream is passed to one of the `send` or `receive` methods along
with an audio processor or message-type `:audio`, an error is signaled.
If no midi devices are provided, an audio-only stream is returned. If such a
stream is passed to one of the `send` or `receive` methods, along with a midi
device or message-type :midi, an error is signaled.

May signal stream-error, portaudio-error, portmidi-error or dsp-error."
  (native-call scl-open-device-stream :stream
    ((midi-input      :midi-device)
     (midi-output     :midi-device)
     (audio-input     :audio-device)
     (audio-output    :audio-device)
     (audio-processor :audio-processor)
     (options         :device-stream-options)) 
    :errors (portmidi-error portaudio-error dsp-error)))

;(defmethod force-start ((obj stream))
;  (native-call scl-stream-force-start :void ((obj :stream)) ))

(defmethod start ((obj stream))
"Starts audio processing."
  (native-call scl-stream-start :void ((obj :stream))
   :errors (portaudio-error portmidi-error dsp-error)))

(defmethod stop ((obj stream))
"Stops audio processing once all current buffers have been drained.
This is the normal way of stopping a stream."
  (native-call scl-stream-stop :void ((obj :stream))
   :errors (portaudio-error portmidi-error dsp-error)))

(defmethod abort ((obj stream))
"Stops audio processing as soon as possible."
  (native-call scl-stream-abort :void ((obj :stream))
   :errors (portaudio-error portmidi-error dsp-error)))

(defmethod running ((obj stream))
  (native-call scl-stream-running :boolean ((obj :stream))))

(defmethod sample-rate ((obj stream))
  (native-call scl-stream-sample-rate :integer ((obj :stream))))

(defmethod audio-buffer-size ((obj stream))
  (native-call scl-stream-audio-buffer-size :integer ((obj :stream))))
   
(defmethod set-error-handler ((obj stream) handler)
  (native-call scl-stream-set-error-handler :void 
    ((obj :stream) 
     ((fli:make-pointer :symbol-name handler) (:function :dummy)))))


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


