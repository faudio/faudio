(defctype Thread :long)

(defctype Mutex :long)

(defctype Condition :long)

(defcfun "createThread" :Doremir.Thread.Thread ())

(defcfun "join" :void (:Doremir.Thread.Thread))

(defcfun "detach" :void (:Doremir.Thread.Thread))

(defcfun "createLock" :Lock ())

(defcfun "destroyLock" :void (:Lock))

(defcfun "lock" :bool (:Doremir.Thread.Mutex))

(defcfun "tryLock" :bool (:Doremir.Thread.Mutex))

(defcfun "unlock" :bool (:Doremir.Thread.Mutex))

(defcfun "createCondition" :Doremir.Thread.Condition ())

(defcfun "waitFor" :void (:Doremir.Thread.Condition))

(defcfun "notify" :void (:Doremir.Thread.Condition))

(defcfun "notifyAll" :void (:Doremir.Thread.Condition))