;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)

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


