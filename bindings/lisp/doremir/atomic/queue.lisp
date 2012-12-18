(defctype Doremir.Atomic.Queue :pointer)

(defcfun "Doremir.Atomic.Queue.create" :Doremir.Atomic.Queue ())

(defcfun "Doremir.Atomic.Queue.destroy" :void (:Doremir.Atomic.Queue))

(defcfun "Doremir.Atomic.Queue.read" :Doremir.Ptr (:Doremir.Atomic.Queue))

(defcfun "Doremir.Atomic.Queue.write" :Doremir.Ptr (:Doremir.Atomic.Queue :Doremir.Ptr))