(defctype Doremir.Thread.Runnable (:pointer :void))

(defctype Doremir.Thread :pointer)

(defctype Doremir.Thread.Mutex :pointer)

(defctype Doremir.Thread.Condition :pointer)

(defcfun "Doremir.Thread.create" :Doremir.Thread ((:pointer :Doremir.Thread.Runnable)))

(defcfun "Doremir.Thread.join" :void (:Doremir.Thread))

(defcfun "Doremir.Thread.detach" :void (:Doremir.Thread))

(defcfun "Doremir.Thread.createMutex" :Doremir.Thread.Mutex ())

(defcfun "Doremir.Thread.destroyMutex" :void (:Doremir.Thread.Mutex))

(defcfun "Doremir.Thread.lock" :boolean (:Doremir.Thread.Mutex))

(defcfun "Doremir.Thread.tryLock" :boolean (:Doremir.Thread.Mutex))

(defcfun "Doremir.Thread.unlock" :boolean (:Doremir.Thread.Mutex))

(defcfun "Doremir.Thread.createCondition" :Doremir.Thread.Condition (:Doremir.Thread.Mutex))

(defcfun "Doremir.Thread.destroyCondition" :void (:Doremir.Thread.Condition))

(defcfun "Doremir.Thread.waitFor" :void (:Doremir.Thread.Condition))

(defcfun "Doremir.Thread.notify" :void (:Doremir.Thread.Condition))

(defcfun "Doremir.Thread.notifyAll" :void (:Doremir.Thread.Condition))