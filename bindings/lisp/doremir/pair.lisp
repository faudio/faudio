(defctype Doremir.Pair.Type :size)

(defctype Doremir.Pair.Unary :Doremir.Ptr)

(defctype Doremir.Pair (:pointer :void))

(defcfun "Doremir.Pair.map" :Doremir.Pair (:Doremir.Pair.Unary :Doremir.Pair))