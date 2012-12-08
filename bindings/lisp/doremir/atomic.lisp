(defctype Atomic :long)

(defctype Updater (:pointer :void))

(defcfun "create" :Doremir.Atomic.Atomic ())

(defcfun "copy" :Doremir.Atomic.Atomic (:Doremir.Atomic.Atomic))

(defcfun "swap" :void (:Doremir.Atomic.Atomic :Doremir.Atomic.Atomic))

(defcfun "destroy" :void (:Doremir.Atomic.Atomic))

(defcfun "get" :long (:Doremir.Atomic.Atomic))

(defcfun "set" :void (:Doremir.Atomic.Atomic :long))

(defcfun "modify" :void (:Doremir.Atomic.Atomic :Doremir.Atomic.Updater))