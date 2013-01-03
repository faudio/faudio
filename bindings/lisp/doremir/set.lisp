(defctype Doremir.Set :pointer)

(defcfun "Doremir.Set.empty" :Doremir.Set ())

(defcfun "Doremir.Set.single" :Doremir.Set (:Doremir.Ptr))

(defcfun "Doremir.Set.add" :Doremir.Set (:Doremir.Ptr :Doremir.Set))

(defcfun "Doremir.Set.remove" :Doremir.Set (:Doremir.Ptr :Doremir.Set))

(defcfun "Doremir.Set.copy" :Doremir.Set (:Doremir.Set))

(defcfun "Doremir.Set.destroy" :void (:Doremir.Set))

(defcfun "Doremir.Set.size" :int (:Doremir.Set))

(defcfun "Doremir.Set.isEmpty" :boolean (:Doremir.Set))

(defcfun "Doremir.Set.isSingle" :boolean (:Doremir.Set))

(defcfun "Doremir.Set.has" :boolean (:Doremir.Ptr :Doremir.Set))

(defcfun "Doremir.Set.isSubsetOf" :boolean (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.isProperSubsetOf" :boolean (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.sum" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.product" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.difference" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.cartesian" :Doremir.Set (:Doremir.Set :Doremir.Set))

(defcfun "Doremir.Set.power" :Doremir.Set (:Doremir.Set))