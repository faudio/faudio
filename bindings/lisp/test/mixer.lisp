
(in-package :faudio)

(audio-end-all-sessions)
(setf se (audio-begin-session))
(setf i (audio-default-input se))
(setf o (audio-default-output se))

(defun smooth (x)
  (signal-loop*
   (lambda (rec)
     (+ (* 0.999 rec)
        (* 0.001 x)))))

;(defun smooth (x) x)

(setf s (audio-open-stream* i o (lambda (_) (cl:list
  (cl:reduce (lambda (x y) (+ x y)) 
    (cl:loop 
      for n from 0 to 16        
      for freq = (cl:* 440 n)
      for bus  = (cl:+ 32 n)
      collect (* 
                 (smooth (input bus)) 
                 (sin (line freq)))))))))

;(audio-schedule (seconds 20) (action-set 32 0.5D0) s)
;(audio-schedule (seconds 55) (action-set 32 0.1D0) s)
;(audio-schedule (seconds 0) (action-set 32 0.0D0) s)
;(destroy s)
;(destroy se)

(cl:print s)

(progn
  (let ((sliders (cl:loop 
                  for n from 0 to 16 
                  for name = (gensym)
                  collect `(,name capi:slider
                                  :start 0
                                  :end 100
                                  :tick-frequency 0
                                  :callback (lambda (interf value type)
                                              (setf x (cl:* (coerce value 'double-float) 0.01D0))
                                              (cl:print x)
                                              (when s
                                                (audio-schedule
                                                 (seconds 0)
                                                 (action-set ,(cl:+ 32 n) x) s))))))
        (other (cl:list `(stop capi:push-button
                               :text "Stop"
                               :callback (lambda (interf value)
                                           (setf s nil)
                                           (setf se nil)
                                           (setf i nil)
                                           (setf o nil)
                                           (audio-end-all-sessions))))))
    (eval `(capi:define-interface demo ()
             ()
             (:panes ,@(append sliders other))
             (:default-initargs :title "Demo")))
    (capi:display (make-instance 'demo))))
