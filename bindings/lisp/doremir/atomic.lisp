(defctype Doremir.Atomic :pointer)

(defctype Doremir.Atomic.Updater (:pointer :void))

(defcfun "Doremir.Atomic.create" :Doremir.Atomic ())

(defcfun "Doremir.Atomic.copy" :Doremir.Atomic (:Doremir.Atomic))

(defcfun "Doremir.Atomic.swap" :void (:Doremir.Atomic :Doremir.Atomic))

(defcfun "Doremir.Atomic.destroy" :void (:Doremir.Atomic))

(defcfun "Doremir.Atomic.get" :pointer (:Doremir.Atomic))

(defcfun "Doremir.Atomic.set" :void (:Doremir.Atomic :pointer))

(defcfun "Doremir.Atomic.modify" :void (:Doremir.Atomic :Doremir.Atomic.Updater))