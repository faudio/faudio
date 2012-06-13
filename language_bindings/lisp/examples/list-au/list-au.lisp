
(require          :asdf)
(asdf:load-system :audio-engine)
(in-package       :audio-engine)

(defconstant +sfpath+ "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/sound.sf2")

(defun send-audio ()
  (let ((opts (default-send-options)))
    (setf (kind opts) :audio) opts))

(defun list-au ()
  (with-library
    (handler-case
      (progn
        (format *standard-output* "Listing Audio Units...~%~%")
        (setf units (load-audio-units))
        (dolist (x units) (print (name x)))
        (format *standard-output* "~%")
        )
      (audio-error (e) (print (message e)))))) 

#-deliver (list-au)