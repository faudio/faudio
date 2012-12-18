(defctype Doremir.Set :pointer)

(defcfun "Doremir.Set.elem" :boolean (:Doremir.Set :Doremir.Ptr))

(defcfun "Doremir.Set.subsetOf" :boolean (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.properSubsetOf" :boolean (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.empty" :Doremir.Set ())

(defcfun "Doremir.Set.add" :Doremir.Set (:Doremir.Set :Doremir.Ptr))

(defcfun "Doremir.Set.remove" :void (:Doremir.Set :Doremir.Ptr))

(defcfun "Doremir.Set.addUnique" :Doremir.Set (:Doremir.Set :Doremir.Ptr))

(defcfun "Doremir.Set.removeUnique" :void (:Doremir.Set :Doremir.Ptr))

(defcfun "Doremir.Set.unionOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.productOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.symmetricDifferenceOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.cartesianProductOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.powerSetOf" :Doremir.Set (:Doremir.Set))