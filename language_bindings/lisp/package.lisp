;;;-*- Mode: lisp -*-

;;
;;  ScoreCleaner Audio Engine
;;
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package #:cl-user)

(defpackage audio-engine

  (:documentation
    "The ScoreCleaner Audio Engine supports cross-platform audio, midi and signal
     processing. This package is the main entry point to the audio engine from Lisp.

     If you are reading this in the source, please see the generated documentation
     in `audio-engine/doc/build`.")

  (:use
    :common-lisp)

  (:shadow
    :error
    :stream
    :abort
    :sequence
    :stream-error)

  (:export
    :object
    
    :error
    :message
    :error-code
    
    :session
    :begin-session     
    :end-session
    :with-session

    :midi-device
    :name
    :host-name
    :has-input-p
    :has-output-p
    :midi-devices
    :default-midi-input-device
    :default-midi-output-device

    :audio-device
    :name
    :host
    :number-of-inputs
    :number-of-outputs
    :sample-rate
    
    :audio-host
    :default-audio-input-device
    :default-audio-output-device
    :audio-hosts
    :default-audio-host
    
    :audio-signal
    :audio-processor
    :name
    :atomicp
    :compoundp
    :statefulp
    :number-of-inputs
    :number-of-outputs
    :number-of-buses
    :number-of-channels
    :pluginp
    :sequence
    :parallel
    :delay
    :feedback
    :fft
    :ifft
    :+
    :-
    :*
    :/
    :%     
    :load-au-plugin
    :load-vst2-plugin
    :make-plugin-processor    
    :load-fluidsynth
    
    :stream
    :realtime-p
    :non-realtime-p
    :has-audio-p
    :has-midi-p
    :start
    :stop
    :abort
    :wait-for           
    :runningp
    :with-running-stream
    :set-error-handler

    :future
    :future-group
    :interrupt
    :interruption-mode
    :disable
    :wait-for
    :donep
    :result

    
    )
  )
  
  
  
  
  
  