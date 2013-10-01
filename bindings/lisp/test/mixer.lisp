
(in-package :faudio)

(setf se (audio-begin-session))
(setf i (audio-default-input se))
(setf o (audio-default-output se))
(setf s (audio-open-stream* i o (lambda (_) (cl:list
                                             
  (+
    (* (input 32) (* 0.1 (sin (line 440))))
    (* (input 33) (* 0.1 (sin (line 550))))
    )

                                             ))))
(audio-send (seconds 20) (action-set 32 0.5D0) s)
(audio-send (seconds 55) (action-set 32 0.1D0) s)
(audio-send (seconds 0) (action-set 32 0.0D0) s)
(destroy s)
(destroy se)


(defvar *foo*)


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
     )
    (:layouts
     ; (main-layout capi:column-layout 
     ;              '(row-of-buttons row-with-editor-pane))
     ; (row-of-buttons capi:row-layout
     ;                 '(page-up page-down open-file))
     ; (row-with-editor-pane capi:row-layout
     ;                       '(viewer))
  )
    
    (:default-initargs :title "Demo"))
  (capi:display (make-instance 'demo))
  )
