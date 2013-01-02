(defctype Doremir.Map :pointer)

(defctype Doremir.Map.Key :Doremir.Ptr)

(defcfun "Doremir.Map.empty" :Doremir.Map ())

(defcfun "Doremir.Map.add" :Doremir.Map (:Doremir.Ptr :Doremir.Map))

(defcfun "Doremir.Map.remove" :void (:Doremir.Ptr :Doremir.Map))

(defcfun "Doremir.Map.copy" :Doremir.Map (:Doremir.Map))

(defcfun "Doremir.Map.destroy" :void (:Doremir.Map))

(defcfun "Doremir.Map.hasKey" :boolean (:Doremir.Map :Doremir.Map.Key))

(defcfun "Doremir.Map.hasElem" :boolean (:Doremir.Map :Doremir.Ptr))

(defcfun "Doremir.Map.hasEntry" :boolean (:Doremir.Map :Doremir.Pair))

(defcfun "Doremir.Map.size" :int (:Doremir.Map))

(defcfun "Doremir.Map.isEmpty" :boolean (:Doremir.Map))

(defcfun "Doremir.Map.isSingle" :boolean (:Doremir.Map))

(defcfun "Doremir.Map.isSubmapOf" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.isProperSubmapOf" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.equal" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.lessThan" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.greaterThan" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.show" :Doremir.String (:Doremir.Map))