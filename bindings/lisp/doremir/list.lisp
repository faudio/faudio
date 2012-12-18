(defctype Doremir.List :pointer)

(defctype Doremir.List.OrdList :Doremir.List)

(defctype Doremir.List.ListList :Doremir.List)

(defcfun "Doremir.List.empty" :Doremir.List ())

(defcfun "Doremir.List.single" :Doremir.List (:Doremir.Ptr))

(defcfun "Doremir.List.cons" :Doremir.List (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.snoc" :Doremir.List (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.append" :Doremir.List (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.copy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.destroy" :void (:Doremir.List))

(defcfun "Doremir.List.isEmpty" :boolean (:Doremir.List))

(defcfun "Doremir.List.length" :int (:Doremir.List))

(defcfun "Doremir.List.head" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.tail" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.init" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.last" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.take" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.drop" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.isElem" :boolean (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.reverse" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sort" :Doremir.List (:Doremir.List.OrdList))

(defcfun "Doremir.List.find" :Doremir.Ptr (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.filter" :Doremir.List (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.any" :boolean (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.all" :boolean (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.map" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.fold" :Doremir.Ptr (:Doremir.Binary :Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.concat" :Doremir.List (:Doremir.List.ListList))

(defcfun "Doremir.List.sum" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.product" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.maximum" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.minimum" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.consd" :Doremir.List (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.snocd" :Doremir.List (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.reversed" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sortd" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.mapd" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.foldd" :Doremir.List (:Doremir.Binary :Doremir.Ptr :Doremir.List))