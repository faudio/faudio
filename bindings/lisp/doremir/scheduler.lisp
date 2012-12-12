(defctype Doremir.Scheduler.Time :int)

(defctype Doremir.Scheduler.Action (:pointer (:pointer :void)))

(defctype Doremir.Scheduler (:pointer :void))

(defcfun "Doremir.Scheduler.create" :Doremir.Scheduler (:Doremir.Thread.Improving))

(defcfun "Doremir.Scheduler.destroy" :void (:Doremir.Scheduler))

(defcfun "Doremir.Scheduler.swap" :void (:Doremir.Scheduler :Doremir.Scheduler))