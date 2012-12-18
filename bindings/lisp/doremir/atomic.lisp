(defctype Doremir.Atomic :pointer)

(defctype Doremir.Atomic.Updater (:pointer (:pointer :void)))

(defcfun "Doremir.Atomic.create" :Doremir.Atomic ())

(defcfun "Doremir.Atomic.copy" :Doremir.Atomic (:Doremir.Atomic))

(defcfun "Doremir.Atomic.swap" :void (:Doremir.Atomic :Doremir.Atomic))

(defcfun "Doremir.Atomic.destroy" :void (:Doremir.Atomic))

(defcfun "Doremir.Atomic.exchange" :boolean (:Doremir.Atomic :Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.Atomic.add" :void (:Doremir.Atomic :Doremir.Ptr))

(defcfun "Doremir.Atomic.get" :Doremir.Ptr (:Doremir.Atomic))

(defcfun "Doremir.Atomic.modify" :void (:Doremir.Atomic :Doremir.Atomic.Updater))

(defcfun "Doremir.Atomic.set" :void (:Doremir.Atomic :Doremir.Ptr))