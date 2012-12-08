(defctype Stack :long)

(defcfun "create" :Doremir.Atomic.Stack.Stack ())

(defcfun "destroy" :void (:Doremir.Atomic.Stack.Stack))

(defcfun "read" :long (:Doremir.Atomic.Stack.Stack))

(defcfun "write" :long (:Doremir.Atomic.Stack.Stack :long))