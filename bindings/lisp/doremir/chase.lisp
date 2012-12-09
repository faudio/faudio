(defctype Doremir.Chase :pointer)

(defctype Doremir.Chase.Value :pointer)

(defcfun "Doremir.Chase.empty" :Doremir.Chase ())

(defcfun "Doremir.Chase.create" :Doremir.Chase (:Doremir.Pair.Type :size))

(defcfun "Doremir.Chase.destroy" :void (:Doremir.Chase))

(defcfun "Doremir.Chase.append" :void (:Doremir.Chase :Doremir.Chase))

(defcfun "Doremir.Chase.isEmpty" :boolean (:Doremir.Chase))

(defcfun "Doremir.Chase.isSingle" :boolean (:Doremir.Chase))

(defcfun "Doremir.Chase.lenght" :int (:Doremir.Chase))

(defctype Doremir.Chase.UnaryFunc :pointer)

(defcfun "Doremir.Chase.map" :Doremir.Chase (:Doremir.Chase.UnaryFunc :Doremir.Chase))

(defcfun "Doremir.Chase.mapDest" :Doremir.Chase (:Doremir.Chase.UnaryFunc :Doremir.Chase))