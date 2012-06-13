;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass audio-processor (object)
  ((num-inputs  :type integer)
   (num-outputs :type integer))
   (:documentation
"An object that transforms audio signals. May be atomic or created by combinators.

See [AudioProcessor](@ref doremir::scl::AudioProcessor)."))

(defmethod name ((obj audio-processor))
  (native-call scl-processor-name :string ((obj :audio-processor))))

(defmethod atomicp ((obj audio-processor))
  (native-call scl-processor-is-atomic :boolean ((obj :audio-processor))))

(defmethod compoundp ((obj audio-processor))
  (native-call scl-processor-is-compound :boolean ((obj :audio-processor))))

(defmethod statefulp ((obj audio-processor))
  (native-call scl-processor-is-stateful :boolean ((obj :audio-processor))))

(defmethod pluginp ((obj audio-processor))
  (native-call scl-processor-is-plugin :boolean ((obj :audio-processor))))

(defmethod num-inputs ((obj audio-processor))
  (native-call scl-processor-num-inputs :integer ((obj :audio-processor))))

(defmethod num-outputs ((obj audio-processor))
  (native-call scl-processor-num-outputs :integer ((obj :audio-processor))))

(defmethod num-buses ((obj audio-processor))
  (native-call scl-processor-num-buses :integer ((obj :audio-processor))))

; (defclass fluidsynth-processor (audio-processor)
;   ()
;   (:documentation
; "See [FluidSynth](@ref doremir::scl::FluidSynth)."))
; 
; (defclass au-processor (audio-processor)
;   ()
;   (:documentation
; "See [AudioUnitProcessor](@ref doremir::scl::AudioUnitProcessor)."))
; 
; (defclass vst-processor (audio-processor)
;   ()
;   (:documentation
; "See [VstProcessor](@ref doremir::scl::VstProcessor)."))


(defun load-fluidsynth (soundfont-path)
"Creates a FluidSynth instance using the given sound font.
May signal dsp-error."
  (native-call scl-load-fluidsynth :audio-processor
    ((soundfont-path :string)) :errors (dsp-error)))
    

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






(defclass audio-plugin (object)
  ((num-inputs  :type integer)
   (num-outputs :type integer))
   (:documentation
"Provides a uniform interface to various audio plug-in architectures such as AU, VST, Ladspa, LV2 etc.  

Each instance of this class corresponds to a named plug-in such as a synth, delay or reverb effect.
Plug-ins can typically be loaded several times, each instance corresponding to an AudioProcessor. The
(make-processor) method provides new instances.

See [AudioPlugin](@ref doremir::scl::AudioPlugin)."))

(defclass audio-plugin-processor (audio-processor)
  ((plugin :type :audio-plugin))) 

(defmethod name ((obj audio-plugin))
  (native-call scl-plugin-name :string ((obj :audio-plugin))))

(defmethod num-inputs ((obj audio-plugin))
  (native-call scl-plugin-num-inputs :integer ((obj :audio-plugin))))

(defmethod num-outputs ((obj audio-plugin))
  (native-call scl-plugin-num-outputs :integer ((obj :audio-plugin))))

(defmethod num-buses ((obj audio-plugin))
  (native-call scl-plugin-num-buses :integer ((obj :audio-plugin))))

(defmethod make-processor ((obj audio-plugin))
  (native-call scl-plugin-create-processor :audio-plugin-processor ((obj :audio-plugin))
  :errors (audio-plugin-error)))

(defmethod plugin ((obj audio-plugin-processor))
"Returns the plugin that created the processor."
  (native-call scl-plugin-from-processor :audio-plugin ((obj :audio-plugin-processor))))

(defun load-audio-units ()
"Returns a list of AudioUnits available on the system."
  (native-call scl-load-audio-units (:list :audio-plugin) ()))

(defun load-dls-music-device ()
"Returns the DLSMusicDevice, also known as the Quicktime Synthesizer."
  (native-call scl-load-dls-music-device :audio-plugin ()))


