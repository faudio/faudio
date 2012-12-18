(defctype Doremir.Atomic.Stack :pointer)

(defcfun "Doremir.Atomic.Stack.create" :Doremir.Atomic.Stack ())

(defcfun "Doremir.Atomic.Stack.destroy" :void (:Doremir.Atomic.Stack))

(defcfun "Doremir.Atomic.Stack.read" :Doremir.Ptr (:Doremir.Atomic.Stack))

(defcfun "Doremir.Atomic.Stack.write" :Doremir.Ptr (:Doremir.Atomic.Stack :Doremir.Ptr))