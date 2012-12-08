(defctype Chase :long)

(defctype Value :long)

(defcfun "empty" :Doremir.Chase.Chase ())

(defcfun "create" :Doremir.Chase.Chase (:Type :long))

(defcfun "destroy" :void (:Doremir.Chase.Chase))

(defcfun "append" :void (:Doremir.Chase.Chase :Doremir.Chase.Chase))

(defcfun "isEmpty" :bool (:Doremir.Chase.Chase))

(defcfun "isSingle" :bool (:Doremir.Chase.Chase))

(defcfun "lenght" :int (:Doremir.Chase.Chase))

(defctype UnaryFunc :long)

(defcfun "map" :Doremir.Chase.Chase (:Doremir.Chase.UnaryFunc :Doremir.Chase.Chase))

(defcfun "mapDest" :Doremir.Chase.Chase (:Doremir.Chase.UnaryFunc :Doremir.Chase.Chase))