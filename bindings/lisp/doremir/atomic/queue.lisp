(defctype Queue :long)

(defcfun "create" :Doremir.Atomic.Queue.Queue ())

(defcfun "destroy" :void (:Doremir.Atomic.Queue.Queue))

(defcfun "read" :long (:Doremir.Atomic.Queue.Queue))

(defcfun "write" :long (:Doremir.Atomic.Queue.Queue :long))