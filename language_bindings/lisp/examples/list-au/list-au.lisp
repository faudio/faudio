
(require          :asdf)
(asdf:load-system :audio-engine)
(in-package       :audio-engine)

(defconstant +sfpath+ "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/sound.sf2")

(defun list-au ()
  (with-library
    (handler-case
      (progn
        (format *standard-output* "Listing Audio Units...~%~%")
        (setf units (load-audio-units))
        (dolist (x units) 
          (format *standard-output* "~s~%" (name x))
          ; (format *standard-output* "  Inputs: ~s~%" (num-inputs x))
          ; (format *standard-output* "  Outputs: ~s~%" (num-outputs x)) ; FIXME
          )
        (format *standard-output* "~%")
        )
      (audio-error (e) (print (message e)))))) 

#-deliver (list-au)