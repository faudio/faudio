(defctype Doremir.Set :pointer)

(defctype Doremir.Set.Value :pointer)

(defcfun "Doremir.Set.elem" :boolean (:Doremir.Set :Doremir.Set.Value))

(defcfun "Doremir.Set.subsetOf" :boolean (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.properSubsetOf" :boolean (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.empty" :Doremir.Set ())

(defcfun "Doremir.Set.add" :Doremir.Set (:Doremir.Set :Doremir.Set.Value))

(defcfun "Doremir.Set.remove" :void (:Doremir.Set :Doremir.Set.Value))

(defcfun "Doremir.Set.addUnique" :Doremir.Set (:Doremir.Set :Doremir.Set.Value))

(defcfun "Doremir.Set.removeUnique" :void (:Doremir.Set :Doremir.Set.Value))

(defcfun "Doremir.Set.unionOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.productOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.symmetricDifferenceOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.cartesianProductOf" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.powerSetOf" :Doremir.Set (:Doremir.Set))