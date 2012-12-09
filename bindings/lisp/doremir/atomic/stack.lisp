(defctype Doremir.Stack :pointer)

(defcfun "Doremir.Atomic.Stack.create" :Doremir.Stack ())

(defcfun "Doremir.Atomic.Stack.destroy" :void (:Doremir.Stack))

(defcfun "Doremir.Atomic.Stack.read" :pointer (:Doremir.Stack))

(defcfun "Doremir.Atomic.Stack.write" :pointer (:Doremir.Stack :pointer))