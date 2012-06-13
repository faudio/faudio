;;;-*- Mode: lisp -*-

;;
;;  ScoreCleaner Audio Engine
;;
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

;(in-package #:cl-user)

(defpackage misc
  (:use :common-lisp))

(defpackage audio-engine

  (:documentation
    "The ScoreCleaner Audio Engine supports cross-platform audio, midi and signal
     processing. This package is the main entry point to the audio engine from Lisp.

     If you are reading this in the source, please see the generated documentation
     in `audio-engine/doc/build`.")

  (:nicknames :audio :ae)
  
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
    
    :audio-error
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
    :load-audio-units
    :load-dls-music-device
    :make-processor    
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
    :running ; FIXME
    :runningp
    :with-running-stream
    :set-error-handler 
    :open-device-stream

    :future
    :future-group
    :interrupt
    :interruption-mode
    :disable
    :wait-for
    :donep
    :result
    
    :device-stream-options
    :default-device-stream-options
    :send-options
    :default-send-options
    :do-now
    :do-later
    :do-at
    :send-now
    :send-later
    :send-at
    :kind

    :load-library    
    :unload-library    
    :with-library
    
    :+type-key-on+
    :+type-key-off+
    :+type-control-change+    
    )
  )
  
  
  
  
  
  