(defctype Doremir.List :pointer)

(defctype Doremir.List.Value :pointer)

(defctype Doremir.List.Func :pointer)

(defctype Doremir.List.Num :pointer)

(defctype Doremir.List.MaybeValue :pointer)

(defctype Doremir.List.MaybeList :pointer)

(defcfun "Doremir.List.empty" :Doremir.List ())

(defcfun "Doremir.List.cons" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.snoc" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.destroy" :void (:Doremir.List))

(defcfun "Doremir.List.copy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.swap" :void (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.isEmpty" :boolean (:Doremir.List))

(defcfun "Doremir.List.lenght" :int (:Doremir.List))

(defcfun "Doremir.List.head" :Doremir.List.MaybeValue (:Doremir.List))

(defcfun "Doremir.List.tail" :Doremir.List.MaybeList (:Doremir.List))

(defcfun "Doremir.List.init" :Doremir.List.MaybeList (:Doremir.List))

(defcfun "Doremir.List.last" :Doremir.List.MaybeValue (:Doremir.List))

(defcfun "Doremir.List.take" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.drop" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.isElem" :boolean (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.reverse" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sort" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sep" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.concat" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.concatSep" :Doremir.List (:Doremir.List))

(defctype Doremir.List.Pred :pointer)

(defcfun "Doremir.List.find" :Doremir.List.MaybeValue (:Doremir.List.Pred :Doremir.List))

(defcfun "Doremir.List.filter" :Doremir.List (:Doremir.List.Pred :Doremir.List))

(defctype Doremir.List.UnaryFunc (:pointer :void))

(defctype Doremir.List.BinaryFunc (:pointer :void))

(defcfun "Doremir.List.map" :Doremir.List (:Doremir.List :Doremir.List.Func))

(defcfun "Doremir.List.foldl" :Doremir.List (:Doremir.List :Doremir.List.BinaryFunc :Doremir.List.Value))

(defcfun "Doremir.List.and" :boolean (:Doremir.List))

(defcfun "Doremir.List.or" :boolean (:Doremir.List))

(defcfun "Doremir.List.any" :boolean (:Doremir.List.Pred :Doremir.List))

(defcfun "Doremir.List.all" :boolean (:Doremir.List.Pred :Doremir.List))

(defcfun "Doremir.List.sum" :Doremir.List.Value (:Doremir.List.Num :Doremir.List))

(defcfun "Doremir.List.product" :Doremir.List.Value (:Doremir.List.Num :Doremir.List))

(defcfun "Doremir.List.maximum" :Doremir.List.Value (:Doremir.List))

(defcfun "Doremir.List.minimum" :Doremir.List.Value (:Doremir.List))

(defcfun "Doremir.List.consDestroy" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.snocDestroy" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.reverseDestroy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sortDestroy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.mapDestroy" :Doremir.List (:Doremir.List :Doremir.List.Func))