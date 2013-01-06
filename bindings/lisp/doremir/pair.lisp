(defctype Doremir.Pair :pointer)

(defcfun "Doremir.Pair.create" :Doremir.Pair (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.Pair.copy" :Doremir.Pair (:Doremir.Pair))

(defcfun "Doremir.Pair.destroy" :void (:Doremir.Pair))

(defcfun "Doremir.Pair.fst" :Doremir.Ptr (:Doremir.Pair))

(defcfun "Doremir.Pair.snd" :Doremir.Ptr (:Doremir.Pair))

(defcfun "Doremir.Pair.dup" :Doremir.Pair (:Doremir.Ptr))

(defcfun "Doremir.Pair.swap" :Doremir.Pair (:Doremir.Pair))

(defcfun "Doremir.Pair.assoc" :Doremir.Pair (:Doremir.Pair))

(defcfun "Doremir.Pair.unassoc" :Doremir.Pair (:Doremir.Pair))