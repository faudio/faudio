
(in-package :audio-engine)

(print-midi-devices)
(print-audio-devices)




(progn
  (format t "~%~30a" "MIDI device")
  (format t "~%~30a ~2a ~3a ~7a" "------------------------------" "--" "---" "-------")
  (dolist (dev (midi-devices))
    (format t "~%~30a ~2a ~3a ~7a"
            (name dev)
            (if (has-input dev) "in" "")
            (if (has-output dev) "out" "")
            (cond
             ((eq dev (default-midi-input-device))   "default in")
             ((eq dev (default-midi-output-device))  "default out")
             (t                                      dev)))))

;; list AUDIO input and output devices
(progn
  (format t "~%~14a ~40a" "AUDIO host" "device")
  (format t "~%~14a ~40a ~2a ~2a ~7a" "--------------" "----------------------------------------" "--" "--" "-------")
  (dolist (host (audio-hosts))
    (dolist (dev (devices host))
      (format t "~%~14a ~40a ~2a ~3a ~7a"
              (name dev)
              (num-inputs dev)
              (num-outputs dev)
              (cond
               ((eq dev (default-audio-input-device))   "default in")
               ((eq dev (default-audio-output-device))  "default out"))))))


(name (default-audio-input-device))
(audio-hosts)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  AUDIO STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fli:print-foreign-modules)

(fli:register-module :scorecleaneraudio
                       :real-name
                       #+:win32 "sclaudio"
                       #+:macosx "@executable_path/../Frameworks/ScoreCleanerAudio.framework/ScoreCleanerAudio"
                       :connection-style :immediate)

(fli:disconnect-module :scorecleaneraudio :remove t)



;;;;;;;;

(defvar *err*)

(handler-bind ((audio-error #'(lambda (err) (setf *err* err))))
   (open-device-stream))

(message *err*)


;; list MIDI input and output devices
(progn
  (format t "~%~30a" "MIDI device")
  (format t "~%~30a ~2a ~3a ~7a" "------------------------------" "--" "---" "-------")
  (dolist (dev (midi-devices))
    (format t "~%~30a ~2a ~3a ~7a"
            (name dev)
            (if (has-input dev) "in" "")
            (if (has-output dev) "out" "")
            (cond
             ((eq dev (default-midi-input-device))   "default in")
             ((eq dev (default-midi-output-device))  "default out")
             (t                                      dev)))))

;; list AUDIO input and output devices
(progn
  (format t "~%~14a ~40a" "AUDIO host" "device")
  (format t "~%~14a ~40a ~2a ~2a ~7a" "--------------" "----------------------------------------" "--" "--" "-------")
  (dolist (host (audio-hosts))
    (dolist (dev (devices host))
      (format t "~%~14a ~40a ~2a ~3a ~7a"
              (name dev)
              (num-inputs dev)
              (num-outputs dev)
              (cond
               ((eq dev (default-audio-input-device))   "default in")
               ((eq dev (default-audio-output-device))  "default out"))))))


(progn
  (setf 
   ;; Windows
;   ai (first (devices (fourth (audio-hosts))))
;   ao (first (devices (fourth (audio-hosts))))
;   ai (fifth (devices (third (audio-hosts))))
;   ao (second (devices (third (audio-hosts))))
;   ai (sixth (devices (third (audio-hosts))))
;   ao (third (devices (third (audio-hosts))))
;   ai (first (devices (second (audio-hosts))))
;   ao (first (devices (second (audio-hosts))))
;   mi (second (midi-devices))
;   mo (fourth (midi-devices))
   ;; Mac OS X
   ai nil
   ao (third (devices (first (audio-hosts))))
   mi nil
   mo nil

  ;(setf fs (load-fluidsynth "C:\\Users\\Sven\\Downloads\\gs_fluidsynth_1.43.sf2"))
  ;(setf fs (load-fluidsynth "C:\\gs_fluidsynth_1.43.sf2"))
;  (setf fs (load-fluidsynth "C:\\Program Files\\LispWorks\\Resources\\sound.sf2"))

  ;(setf fs (load-fluidsynth "/Users/sven/DoReMIR/x86-win32/gs_fluidsynth_1.43.sf2"))
   fs (load-fluidsynth "/Applications/LispWorks 6.1/LispWorks.app/Contents/Resources/sound.sf2")
   )

  (setf opts (default-send-options))
  (setf (kind opts) :audio) 

  (setf s (open-device-stream
           :audio-input ai
           :audio-output ao
           :midi-input mi
           :midi-output mo
           :audio-processor fs)))


(mapcar 'name (list ai ao mi mo))

(handler-case 
    (start s)
  (audio-error (error) (print (message error))))

(running s)
(stop s)

(progn
  (send-now s '(144 64 120) opts)
  (send-now s '(144 66 120) opts)
  (send-now s '(144 68 120) opts)
  (send-now s '(144 73 120) opts))
(send-now s '(144 64 120))
(send-now s '(144 60 120))

(dotimes (i 100)
  (send-later s (* i 100) `(144 ,(+ 55 (random 12)) 120) opts))

(define-receiver play-in-fluidsynth (time msg) nil)
  (send-now s msg opts))

(handler-case
    (setf receive-midi-input-2 (receive s 'play-in-fluidsynth))
  (audio-error (e) (print (message e))))

(interrupt receive-midi-input-2)

(interrupt receive-midi-input-2)

(let ((dso (default-send-options)))
  (format t "~%default-send-options:~%  kind: ~a~%  processors: ~a~%  devices ~a~%  channels: ~a"
          (kind dso)
          (processors dso)
          (devices dso)
          (channels dso)))

(setf s (open-device-stream))
(setf s (open-device-stream
         :midi-input (first (midi-devices))
         :midi-output (second (midi-devices))))

(handler-bind ((audio-error #'(lambda (err) (print (message err)))))
  (start s))


(defun scale (x)
  (case x (0 0)
          (1 2)
          (2 4)
          (3 5)
          (4 7)
          (5 9)))

(dotimes (i 10500)
  (send-later s (* 100 i) `(144 ,(+ 66 (scale (random 6))) ,(floor (+ 80 (cos (mod i 3))))) opts))


(defvar *time* 0)
(define-action set-time (time)
  (setf *time* time))

(do-now s 'set-time)
(print *time*)


(start s)

(running s)
(stop s)
(abort s)

(send-now s '(144 60 120))
(send-now s '(144 60 0))

(progn
  (send-later s 0 '(144 60 80))
  (send-later s 1000 '(144 62 100))
  (send-later s 2000 '(144 64 120))
  (send-later s 4000 '(144 60 0))
  (send-later s 4000 '(144 62 0))
  (send-later s 4000 '(144 64 0))
  )

(defvar *futures* nil)
*futures*

(progn
  (setf *futures* nil)
  (push (send-later midi::*listener-audio-stream* 0 '(144 60 80)) *futures*)
  (push (send-later midi::*listener-audio-stream* 1000 '(144 62 100)) *futures*)
  (push (send-later midi::*listener-audio-stream* 2000 '(144 64 120)) *futures*)
  (push (send-later midi::*listener-audio-stream* 4000 '(144 60 0)) *futures*)
  (push (send-later midi::*listener-audio-stream* 4000 '(144 62 0)) *futures*)
  (push (send-later midi::*listener-audio-stream* 4000 '(144 64 0)) *futures*)
  )

(start midi::*listener-audio-stream*)
(stop midi::*listener-audio-stream*)
(abort midi::*listener-audio-stream*)
(running midi::*listener-audio-stream*)

(dolist (f *futures*)
  (interrupt f))

(defun send-note (note-number duration velocity)
  (send-now s `(144 ,note-number ,velocity))
  (send-later s duration `(144 ,note-number 0)))

(defun send-note (note-number)
  (send-now s (list 144 note-number 100)))

(send-note 54 500 100)


(defvar *messages* nil)
(print *messages*)

(define-receiver my-buffering-receiver (time msg)
  (midi::write-to-log-file :debug "my-buffering-receiver ~a ~a" time msg)
  (push-list *messages* (list time msg)))

(setf receive-midi-input (receive s 'my-buffering-receiver))

(do ((x 0 (pop-list *messages*))) ((eq nil *messages*)) (print (car *messages*)))


(interrupt receive-midi-input)

(defvar *receiver* 0)
(defvar *playing-finished* nil)

(define-action just-do-it (time)
  (midi::write-to-log-file :info "receive at ~a" time))

(define-action ink (time)
  (incf *receiver*))

(do-now s 'just-do-it)
(do-now s 'ink)

(print *receiver*)

(progn
  (do-now s 'just-do-it)
  (do-later s 0 'just-do-it)
  (do-later s 1000 'just-do-it)
  (do-later s 2000 'just-do-it)
  (do-later s 3000 'just-do-it)
  (do-later s 4000 'just-do-it)
  (do-later s 5000 'just-do-it)
  (do-later s 6000 'just-do-it)
  (do-later s 7000 'just-do-it)
  (do-later s 8000 'just-do-it)
  (do-later s 9000 'just-do-it))

(let ((futures-list nil))
  (loop for i from 0 to 10 do
        (push (do-later s (* i 1000) 'just-do-it) futures-list))
  (sleep 5)
  (loop for x in futures-list do
        (interrupt x)))
      



;;;;;;;; AUDIO

(num-inputs (default-audio-input-device (default-audio-host)))
(sample-rate (first (devices (default-audio-host))))

(dolist (h (audio-hosts)) (print (name h)))

(name (default-audio-host))
(name (default-audio-input-device (default-audio-host)))
(name (default-audio-output-device (default-audio-host)))

(dolist (dev (devices (default-audio-host)))
  (print (name dev)))


;;;;;;;; STREAM

(setf s (open-device-stream))

(setf s (open-device-stream 
  :midi-input (first (midi-devices))
  :midi-output (second (midi-devices))))

(setf s (open-device-stream 
  :midi-input (second (midi-devices))
  :midi-output (fourth (midi-devices))))

(running s)

(start s)
(stop s)
(abort s)


;;;;;;;;

(defvar *counter* 0)
(defvar *time* 0)

(fli:define-foreign-callable ("listener" :result-type :void :calling-convention :cdecl) ((time :int))
  (setf *counter* (1+ *counter*))
  (setf *time* time)
)
(setf *listenerf* (fli:make-pointer :symbol-name :listener))

(print (list *counter* *time*))


(native-call 
   scl-schedule-now :future
   (
     (s :stream)
     (*listenerf* (:function :dummy))
     (nil :object)
   ))
(native-call 
   scl-schedule-later :future
   (
     (s :stream)
     (*listenerf* (:function :dummy))
     (5000 :integer)
     (nil :object)
   ))
(native-call 
   scl-schedule-at :future
   (
     (s :stream)
     (*listenerf* (:function :dummy))
     (35000 :integer)
     (nil :object)
   ))


;;;;;;;;

(defun play (pitch vel)
  (native-call
  scl-send-now :future
  (
    (s :stream)
    ((list 144 pitch vel) (:list :atom))
    (nil :object)
  )))

(play 60 120)
(play 60 0)

(dotimes (i 20) (play (+ 60 i) 60))
(dotimes (i 20) (play (+ 60 i) 0))


(native-call
  scl-send-later :future
  (
    (s :stream)
    (500 :integer)
    ('(144 60 100) (:list :atom))
    (nil :object)
  ))


(native-call scl-test-pass-list-atom :void 
  (
    ('("hello" "my" 1 2 3 5 6 7) (:list :atom))
  ))







;;;;;;;; TO ECHO MIDI

(defun convert-atom-list (type array length)
  (setf res nil)
  (dotimes (i length)
    (push-list res (eval (import-form type length (fli:dereference array))))
    (fli:incf-pointer array))
  (reverse res)))


(defvar *messages* nil)


(fli:define-foreign-callable ("receiver" :result-type :void :calling-convention :cdecl)
  ((time :int) (array (:pointer (:pointer :void))) (length :int))
  (let (( msg (convert-atom-list :atom array length) ))
  (push-list *messages* (list time msg)) )
)

(setf *receivef* (fli:make-pointer :symbol-name :receiver))

(print *messages*)
(do ((x 0 (pop-list *messages*))) ((eq nil *messages*)) (print (car *messages*)))


;(convert-atom-list :atom *last-array* *last-length*)



(setf receive-midi-input (native-call 
   scl-receive :future
   (
     (s :stream)
     (*receivef* (:function :dummy))
     (nil :object)
   )))


(interrupt receive-midi-input)



;;;;


(defmethod foo (&key (val 3)) val)
(foo)


(defun foo (&key (a 0) (b 0))
 (+ a b))

(foo)





