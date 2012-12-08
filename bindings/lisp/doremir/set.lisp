(defctype Set :long)

(defctype Value :long)

(defcfun "elem" :bool (:Doremir.Set.Set :Doremir.Set.Value))

(defcfun "subsetOf" :bool (:Doremir.Set.Set :Doremir.Set.Set))

(defcfun "properSubsetOf" :bool (:Doremir.Set.Set :Doremir.Set.Set))

(defcfun "empty" :Doremir.Set.Set ())

(defcfun "add" :Doremir.Set.Set (:Doremir.Set.Set :Doremir.Set.Value))

(defcfun "remove" :void (:Doremir.Set.Set :Doremir.Set.Value))

(defcfun "addUnique" :Doremir.Set.Set (:Doremir.Set.Set :Doremir.Set.Value))

(defcfun "removeUnique" :void (:Doremir.Set.Set :Doremir.Set.Value))

(defcfun "unionOf" :Doremir.Set.Set (:Doremir.Set.Set :Doremir.Set.Set))

(defcfun "productOf" :Doremir.Set.Set (:Doremir.Set.Set :Doremir.Set.Set))

(defcfun "symmetricDifferenceOf" :Doremir.Set.Set (:Doremir.Set.Set :Doremir.Set.Set))

(defcfun "cartesianProductOf" :Doremir.Set.Set (:Doremir.Set.Set :Doremir.Set.Set))

(defcfun "powerSetOf" :Doremir.Set.Set (:Doremir.Set.Set))