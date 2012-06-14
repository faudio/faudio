
(require          :asdf)
(asdf:load-system :audio-engine)
(in-package       :audio-engine)

(defun list-midi ()
  (with-library
    (handler-case
      (let* ((devices (midi-devices)))
        (format *standard-output* "Listing Midi devices...~%~%")
          (dolist (x devices)
            (format *standard-output* "~s~%" (name x))
            (format *standard-output* "  Host: ~s~%" (host-name x))
            (format *standard-output* "  Input: ~s~%" (has-input x))
            (format *standard-output* "  Output: ~s~%" (has-output x))
          ))
      (audio-error (e) (print (message e))))))

#-deliver (list-midi)