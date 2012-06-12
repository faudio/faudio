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


