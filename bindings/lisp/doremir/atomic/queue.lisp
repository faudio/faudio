(defctype Doremir.Queue :pointer)

(defcfun "Doremir.Atomic.Queue.create" :Doremir.Queue ())

(defcfun "Doremir.Atomic.Queue.destroy" :void (:Doremir.Queue))

(defcfun "Doremir.Atomic.Queue.read" :pointer (:Doremir.Queue))

(defcfun "Doremir.Atomic.Queue.write" :pointer (:Doremir.Queue :pointer))