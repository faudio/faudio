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


