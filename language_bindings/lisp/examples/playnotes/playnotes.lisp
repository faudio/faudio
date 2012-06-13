
(require :asdf)
(asdf:load-system :audio-engine)
(audio-engine:load-library)

(in-package :audio-engine)

(defconstant +sfpath+ "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/sound.sf2")

(defun send-audio ()
  (let ((opts (default-send-options)))
    (setf (kind opts) :audio) opts))

(defun main ()
  (handler-case  
    (let*
      ((input-dev  
        (audio-engine:default-audio-input-device))
      (output-dev 
        (audio-engine:default-audio-output-device))
      (proc       
         (load-fluidsynth +sfpath+))     
      (stream (open-device-stream 
        :audio-input input-dev
        :audio-output output-dev
        :audio-processor proc)))
      (format *standard-output* "Starting audio...~%")
      (start stream)        
      (send-later stream 0    '(#xc0 11)     (send-audio))
      (send-later stream 200  '(#x90 60 100) (send-audio))
      (send-later stream 400  '(#x90 62 100) (send-audio))
      (send-later stream 600  '(#x90 63 100) (send-audio))
      (send-later stream 4000 '(#x90 60 0)   (send-audio))
      (send-later stream 4000 '(#x90 62 0)   (send-audio))
      (send-later stream 4000 '(#x90 63 0)   (send-audio))
      (sleep 5)
      (stop stream))
    (audio-error (e) (print (message e)))))
    
#-deliver (main)