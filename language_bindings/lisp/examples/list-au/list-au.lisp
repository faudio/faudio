
(require          :asdf)
(asdf:load-system :audio-engine)
(in-package       :audio-engine)

(defun list-au ()
  (with-library
    (handler-case
      (let* ((units (load-audio-units)))
        (format *standard-output* "Listing Audio Units...~%~%")
        (dolist (x units) 
          (format *standard-output* "~s~%" (name x))
          ; (format *standard-output* "  Inputs: ~s~%" (num-inputs x))
          ; (format *standard-output* "  Outputs: ~s~%" (num-outputs x)) ; FIXME
          )
        (format *standard-output* "~%")
        )
      (audio-error (e) (print (message e)))))) 

#-deliver (list-au)