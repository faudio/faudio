;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)

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

