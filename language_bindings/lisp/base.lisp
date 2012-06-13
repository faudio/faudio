;;;-*- Mode: lisp -*-

;;                                               
;;  ScoreCleaner Audio Engine
;;  
;;  Copyright (c) 2012 DoReMIR Music Research AB.
;;  All rights reserved
;;

(in-package :audio-engine)

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
  (:documentation "An error related to a stream.")) ; TODO get stream

(define-condition dsp-error (audio-error) ()
  (:documentation "An error related to an audio processor."))

(define-condition plugin-error ()
  ((plugin    :type :audio-plugin)
   (processor :type :audio-processor))
  (:documentation "An error related to an audio plugin."))

(defmethod message ((obj audio-error))
"A string describing the error."
  (native-call scl-error-message :string ((obj :error))))

(defmethod error-code ((obj portmidi-error))
"Portmidi-specific error code"
  (native-call scl-portmidi-error-code :int ((obj :portmidi-error))))

(defmethod error-code ((obj portaudio-error))
"Portaudio-specific error code"
  (native-call scl-portaudio-error-code :int ((obj :portaudio-error))))

(defmethod plugin ((obj plugin-error))
  (native-call scl-plugin-from-error :audio-plugin ((obj :plugin-error))))
