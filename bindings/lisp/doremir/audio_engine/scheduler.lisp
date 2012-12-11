(defctype Doremir.AudioEngine.Scheduler.Time :int)

(defctype Doremir.AudioEngine.Scheduler.Action (:pointer (:pointer :void)))

(defctype Doremir.Scheduler (:pointer :void))

(defcfun "Doremir.AudioEngine.Scheduler.create" :Doremir.Scheduler (:Doremir.Improving))

(defcfun "Doremir.AudioEngine.Scheduler.destroy" :void (:Doremir.Scheduler))

(defcfun "Doremir.AudioEngine.Scheduler.swap" :void (:Doremir.Scheduler :Doremir.Scheduler))