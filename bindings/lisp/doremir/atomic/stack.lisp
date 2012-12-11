(defctype Doremir.Atomic.Stack :pointer)

(defcfun "Doremir.Atomic.Stack.create" :Doremir.Atomic.Stack ())

(defcfun "Doremir.Atomic.Stack.destroy" :void (:Doremir.Atomic.Stack))

(defcfun "Doremir.Atomic.Stack.read" :pointer (:Doremir.Atomic.Stack))

(defcfun "Doremir.Atomic.Stack.write" :pointer (:Doremir.Atomic.Stack :pointer))