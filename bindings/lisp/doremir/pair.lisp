(defctype Doremir.Pair.Type :size)

(defctype Doremir.Pair.Func :pointer)

(defctype Doremir.Pair (:pointer :void))

(defcfun "Doremir.Pair.map" :Doremir.Pair (:Doremir.Pair.Func :Doremir.Pair))