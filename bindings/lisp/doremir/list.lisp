(defctype Doremir.List :pointer)

(defcfun "Doremir.List.empty" :Doremir.List ())

(defcfun "Doremir.List.single" :Doremir.List (:Doremir.Ptr))

(defcfun "Doremir.List.cons" :Doremir.List (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.append" :Doremir.List (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.copy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.destroy" :void (:Doremir.List))

(defcfun "Doremir.List.isEmpty" :boolean (:Doremir.List))

(defcfun "Doremir.List.isSingle" :boolean (:Doremir.List))

(defcfun "Doremir.List.length" :int (:Doremir.List))

(defcfun "Doremir.List.head" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.tail" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.init" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.last" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.take" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.drop" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.has" :boolean (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.find" :Doremir.Ptr (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.filter" :Doremir.List (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.reverse" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sort" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.map" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.foldLeft" :Doremir.Ptr (:Doremir.Binary :Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.concat" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.equal" :boolean (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.lessThan" :boolean (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.greaterThan" :boolean (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.show" :Doremir.String (:Doremir.List))