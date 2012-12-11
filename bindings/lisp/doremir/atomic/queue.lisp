(defctype Doremir.Atomic.Queue :pointer)

(defcfun "Doremir.Atomic.Queue.create" :Doremir.Atomic.Queue ())

(defcfun "Doremir.Atomic.Queue.destroy" :void (:Doremir.Atomic.Queue))

(defcfun "Doremir.Atomic.Queue.read" :pointer (:Doremir.Atomic.Queue))

(defcfun "Doremir.Atomic.Queue.write" :pointer (:Doremir.Atomic.Queue :pointer))