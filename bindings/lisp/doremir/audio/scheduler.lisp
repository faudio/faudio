(defctype Doremir.Audio.Scheduler.Time :int)

(defctype Doremir.Audio.Scheduler.Action (:pointer (:pointer :void)))

(defctype Doremir.Scheduler (:pointer :void))

(defcfun "Doremir.Audio.Scheduler.create" :Doremir.Scheduler (:Doremir.Improving))

(defcfun "Doremir.Audio.Scheduler.destroy" :void (:Doremir.Scheduler))

(defcfun "Doremir.Audio.Scheduler.swap" :void (:Doremir.Scheduler :Doremir.Scheduler))