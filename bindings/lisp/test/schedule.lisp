

(defvar *audio-schedule-lock* (mp:make-lock :name "Audio Scheduler Lock"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  push-action
;;
;;  Push one ore more actions onto a list, calling faudio::action-send for each one.
;;  The action argument can be a single message, or a list of messages
;;
;;  Example: (push-action (faudio::midi #x90 note-number velocity) delta-time future-list)
;;

(defmacro push-action (msg time place)
 (let ((_msg (gensym "msg"))
       (_time (gensym "delta-time"))
       (_loop-var (gensym "loop-var")))
   `(let ((,_msg ,msg)
          (,_time ,time))
      (when ,_msg
        (unless (listp ,_msg)
          (setf ,_msg (list ,_msg)))
        (loop for ,_loop-var in ,_msg do
              (push (cons (ensure-action ,_loop-var) ,_time) ,place))))))


;; Helper for audio-schedule and audio-schedule-relative

(defun ensure-action (a)
 (typecase a
   (faudio::action a)
   (faudio::midi-message (mp:with-lock (*audio-schedule-lock*) (faudio::action-send (if (string= *output-type* "audio") "DLS" "") a)))
   (t (error "Cannot make ~a into an action" a))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  audio-schedule
;;
;;  Thread-safe scheduling wrapper. The action argument can be a single action
;;  or a list of actions, the time argument is an absolute time in milliseconds.
;;  Schedules the action on *audio-stream* or *midi-output-stream*
;;  depending on the *current-output*.
;;

(defun audio-schedule (time action)
 (when action
   (unless (listp action)
     (setf action (list action)))
   (mp:with-lock (*audio-schedule-lock*)
     (if (string= *output-type* "audio")
         (loop for a in action do
               (faudio::audio-schedule (faudio::milliseconds (round time)) (ensure-action a) *audio-stream*))
       (loop for a in action do
             (faudio::midi-schedule (faudio::milliseconds (round time)) (ensure-action a) *midi-output-stream*))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  audio-schedule-relative
;;
;;  Thread-safe scheduling wrapper. The action argument can be a single action
;;  or a list of actions, the time argument is a time in milliseconds, relative from now.
;;  Schedules the action on *audio-stream* or *midi-output-stream*
;;  depending on the *current-output*.
;;

(defun audio-schedule-relative (time-from-now action)
 (when action
   (unless (listp action)
     (setf action (list action)))
   (mp:with-lock (*audio-schedule-lock*)
     (if (string= *output-type* "audio")
         (loop for a in action do
               (faudio::audio-schedule-relative (faudio::milliseconds (round time-from-now)) (ensure-action a) *audio-stream*))
       (loop for a in action do
             (faudio::midi-schedule-relative (faudio::milliseconds (round time-from-now)) (ensure-action a) *midi-output-stream*))))))

(defun audio-send-now (action)
 (audio-schedule-relative 0 action))


(defun audio-send-note-now (channel note-number velocity duration)
 (audio-send-now (make-action-many (list (cons (ensure-action (note-on-message channel note-number velocity)) 0)
                                         (cons (ensure-action (note-off-message channel note-number)) duration)))))


(defun note-on-message (channel note-number velocity)
 (faudio::midi (+ #x90 channel) note-number velocity))

(defun note-off-message (channel note-number)
 (faudio::midi (+ #x90 channel) note-number 0))


(defun test-faudio-initialize ()
 ;; register-scorecleaner-audio-framework
 (let ((framework-path (truename (merge-pathnames (make-pathname :directory '(:relative "Frameworks"))
                                                  (merge-pathnames (make-pathname :directory '(:relative :up))
                                                                   (make-pathname :directory (pathname-directory (lw:lisp-image-name)))))))
       (framework-name "Faudio"))
   (push framework-path  cffi:*darwin-framework-directories*)
   (cffi:load-foreign-library `(:framework ,framework-name)))

 (setf *output-type* "audio"
       *play-clicks* t)

 ;; initialize-audio
 (faudio::fa-set-log-file (format nil "~a/Library/Logs/Faudio.log" (user-homedir-pathname)))
 (faudio::fa-initialize)

 (let ((device-host-name "Core Audio")
       (device-name "Built-in Output"))

   ;; start audio session
   (setf *audio-session* (faudio::audio-begin-session))

   ;; register audio status change callback
   ; (register-audio-status-change-callback *audio-session*)

   ;; find audio devices
   ; (multiple-value-setq (*audio-input-devices* *audio-output-devices*)
       ; (audio-input-and-output-devices *audio-session*))
  (setf *audio-input-devices* (cl:list (faudio::audio-default-input *audio-session*)))
  (setf *audio-output-devices* (cl:list (faudio::audio-default-output *audio-session*)))

   (setf *current-output* (find (list device-host-name device-name) *audio-output-devices*
                                    :test #'(lambda (x y) (and (string= (first x) (faudio::audio-host-name y))
                                                               (string= (second x) (faudio::audio-name y))))))
   (assert (typep *current-output* 'faudio::audio-device))

   (setf *audio-stream* (faudio::audio-open-stream*
                (faudio::audio-default-input *audio-session*)
                *current-output*
                (lambda (inputs)
                  (declare (ignorable inputs))
                  (mapcar 
                   (lambda (x) (faudio::* x 1)) 
                   (faudio::signal-dls*)))))

   (assert *audio-stream*)

   (setf *global-clock* (faudio::audio-stream-clock *audio-stream*))))


(defun test-faudio ()

 (let ((futures nil)
       (clicks nil)
       (ms 0)
       (clock-start (+ (faudio::clock-milliseconds *global-clock*) 500)))
   (labels ((push-note-on (time channel note-number velocity)
              (push-action (faudio::midi (+ #x90 channel) note-number velocity) time futures))
            (push-note-off (time channel note-number)
              (push-note-on time channel note-number 0))
            (push-click-track-note (time channel)
              (push-action (faudio::action-while* #'(lambda (x) (declare (ignorable x)) *play-clicks*) (ensure-action (faudio::midi (+ #x90 channel) 76 120))) time clicks)
              (push-action (faudio::action-while* #'(lambda (x) (declare (ignorable x)) *play-clicks*) (ensure-action (faudio::midi (+ #x90 channel) 76 0))) (+ time 50) clicks)))

     (let ((note-channel 0)
           (click-channel 9))

       (loop repeat 8 with pitch = 60 do

             (push-click-track-note ms click-channel)

             (push-note-on ms note-channel pitch 120)
             (incf ms 250)
             (push-note-off ms note-channel pitch)
             (incf ms 50)
             (incf pitch)

             (push-note-on ms note-channel pitch 120)
             (incf ms 250)
             (push-note-off ms note-channel pitch)
             (incf ms 50)
             (incf pitch)

             (push-note-on ms note-channel pitch 120)
             (incf ms 250)
             (push-note-off ms note-channel pitch)
             (incf ms 50)
             (incf pitch)))

     (setf futures (nreverse futures)
           clicks (nreverse clicks))
     (write-to-log-file :debug "futures: ~a" futures)
     (write-to-log-file :debug "clicks: ~a" clicks)
     (audio-schedule clock-start (make-action-many futures))
     (audio-schedule clock-start (make-action-many clicks))


     (incf ms 1000)

     (let ((note-channel 0)
           (click-channel 9))

       (loop repeat 8 with pitch = 60 do

             (push-click-track-note ms click-channel)

             (push-note-on ms note-channel pitch 120)
             (incf ms 250)
             (push-note-off ms note-channel pitch)
             (incf ms 1) ;; <----  Verkar som att ljudmotorn inte kan hantera små intervall mellan actions
             (incf pitch)

             (push-note-on ms note-channel pitch 120)
             (incf ms 250)
             (push-note-off ms note-channel pitch)
             (incf ms 50)
             (incf pitch)

             ;(incf ms 151/3)

             (push-note-on ms note-channel pitch 120)
             (incf ms 250)
             (push-note-off ms note-channel pitch)
             (incf ms 50)
             (incf pitch)))

     (setf futures (nreverse futures)
           clicks (nreverse clicks))
     (write-to-log-file :debug "futures: ~a" futures)
     (write-to-log-file :debug "clicks: ~a" clicks)
     (audio-schedule clock-start (make-action-many futures))
     (audio-schedule clock-start (make-action-many clicks))
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function make-action-many
;;
;;  Takes a list of conses where the first value is an faudio::action-send and the second value
;;  is an absolute time "before" the action.
;;
;;  Example input:
;;  '((action1 . 5) (action2 . 20) (action3 . 50) (action4 . 100))
;;
;;  The result is an faudio::action-many on the following format, where the absolute "before" times
;;  have been translated into "after" delta times.
;;
;;  Example result:
;;  (faudio::action-many '((faudio::pair faudio::action-null (faudio::millisecond 5))
;;                         (faudio::pair action1 (faudio::millisecond 15))
;;                         (faudio::pair action2 (faudio::millisecond 30))
;;                         (faudio::pair action3 (faudio::millisecond 50))
;;                         (faudio::pair action4 (faudio::millisecond 0))))
;;

(defun make-action-many (l)
 (setf l (stable-sort l #'< :key #'cdr))
 (write-to-log-file :debug "make-action-many")
 (faudio::action-many 
  (concatenate 'list
               (if (and l (plusp (cdar l))) (list (faudio::pair-create (faudio::action-null) (faudio::milliseconds (cdar l)))))
               (maplist #'(lambda (x)
                            (let ((a (caar x))
                                  (b (if (and (cdadr x) (cdar x))
                                         (- (cdadr x) (cdar x))
                                       0)))
                              (faudio::pair-create (ensure-action a) (faudio::milliseconds b))))
                        l))))




(defun write-to-log-file (&rest _))
