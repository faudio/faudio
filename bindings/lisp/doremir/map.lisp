(defctype Doremir.Map :pointer)

(defctype Doremir.Map.Key :Doremir.Ptr)

(defcfun "Doremir.Map.empty" :Doremir.Map ())

(defcfun "Doremir.Map.add" :Doremir.Map (:Doremir.Pair :Doremir.Map))

(defcfun "Doremir.Map.remove" :void (:Doremir.Pair :Doremir.Map))

(defcfun "Doremir.Map.copy" :Doremir.Map (:Doremir.Map))

(defcfun "Doremir.Map.destroy" :void (:Doremir.Map))

(defcfun "Doremir.Map.fromPair" :Doremir.Map (:Doremir.Pair))

(defcfun "Doremir.Map.fromList" :Doremir.Map (:Doremir.Pair))

(defcfun "Doremir.Map.size" :int (:Doremir.Map))

(defcfun "Doremir.Map.isEmpty" :boolean (:Doremir.Map))

(defcfun "Doremir.Map.isSingle" :boolean (:Doremir.Map))

(defcfun "Doremir.Map.get" :Doremir.Ptr (:Doremir.Map.Key :Doremir.Map))

(defcfun "Doremir.Map.hasKey" :boolean (:Doremir.Map.Key :Doremir.Map))

(defcfun "Doremir.Map.hasElem" :boolean (:Doremir.Ptr :Doremir.Map))

(defcfun "Doremir.Map.hasEntry" :boolean (:Doremir.Pair :Doremir.Map))

(defcfun "Doremir.Map.isSubmapOf" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.isProperSubmapOf" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.sum" :Doremir.Map (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.product" :Doremir.Map (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.difference" :Doremir.Map (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.cartesian" :Doremir.Map (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.power" :Doremir.Map (:Doremir.Map))