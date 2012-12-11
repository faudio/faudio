(defctype Doremir.AudioEngine.Scheduler.Time :int)

(defctype Doremir.AudioEngine.Scheduler.Action (:pointer (:pointer :void)))

(defctype Doremir.Audio.Engine.Scheduler (:pointer :void))

(defcfun "Doremir.AudioEngine.Scheduler.create" :Doremir.Audio.Engine.Scheduler (:Doremir.Thread.Improving))

(defcfun "Doremir.AudioEngine.Scheduler.destroy" :void (:Doremir.Audio.Engine.Scheduler))

(defcfun "Doremir.AudioEngine.Scheduler.swap" :void (:Doremir.Audio.Engine.Scheduler :Doremir.Audio.Engine.Scheduler))