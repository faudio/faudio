
(in-package :faudio)

(setf se (audio-begin-session))
(setf i (audio-default-input se))
(setf o (audio-default-output se))
(defun smooth (x)
  (signal-loop*
   (lambda (rec)
     (+ (* 0.999 rec)
        (* 0.001 x)))))
(setf s (audio-open-stream* i o (lambda (_) (cl:list

  (+
    (+
      (* (smooth (input 32)) (* 0.1 (sin (line 440))))
      (* (smooth (input 33)) (* 0.1 (sin (line 550))))
      )
    (+
      (* (smooth (input 34)) (* 0.1 (sin (line 660))))
      (* (smooth (input 35)) (* 0.1 (sin (line 770))))
      ))

                                             ))))
(audio-send (seconds 20) (action-set 32 0.5D0) s)
(audio-send (seconds 55) (action-set 32 0.1D0) s)
(audio-send (seconds 0) (action-set 32 0.0D0) s)
(destroy s)
(destroy se)



(progn
  (capi:define-interface demo ()
    ()
    (:panes
     (slider1 capi:slider
              :start 0
              :end 100
              :tick-frequency 0
              :callback (lambda (interf value type)
                  (setf x (cl:* (coerce value 'double-float) 0.01D0))
                  (cl:print x)
                  (audio-send
                   (seconds 0)
                   (action-set 32 x) s)))
     (slider2 capi:slider
              :start 0
              :end 100
              :tick-frequency 0
              :callback (lambda (interf value type)
                  (setf x (cl:* (coerce value 'double-float) 0.01D0))
                  (cl:print x)
                  (audio-send
                   (seconds 0)
                   (action-set 33 x) s)))
     (slider3 capi:slider
              :start 0
              :end 100
              :tick-frequency 0
              :callback (lambda (interf value type)
                  (setf x (cl:* (coerce value 'double-float) 0.01D0))
                  (cl:print x)
                  (audio-send
                   (seconds 0)
                   (action-set 34 x) s)))
     (slider4 capi:slider
              :start 0
              :end 100
              :tick-frequency 0
              :callback (lambda (interf value type)
                  (setf x (cl:* (coerce value 'double-float) 0.01D0))
                  (cl:print x)
                  (audio-send
                   (seconds 0)
                   (action-set 35 x) s)))
     )
    (:default-initargs :title "Demo"))
  (capi:display (make-instance 'demo))
  )
