(defctype List :long)

(defctype Value :long)

(defcfun "empty" :Doremir.List.List ())

(defcfun "cons" :Doremir.List.List (:Doremir.List.Value :Doremir.List.List))

(defcfun "snoc" :Doremir.List.List (:Doremir.List.Value :Doremir.List.List))

(defcfun "destroy" :void (:Doremir.List.List))

(defcfun "copy" :Doremir.List.List (:Doremir.List.List))

(defcfun "swap" :void (:Doremir.List.List :Doremir.List.List))

(defcfun "isEmpty" :bool (:Doremir.List.List))

(defcfun "lenght" :int (:Doremir.List.List))

(defcfun "head" :MaybeValue (:Doremir.List.List))

(defcfun "tail" :MaybeList (:Doremir.List.List))

(defcfun "init" :MaybeList (:Doremir.List.List))

(defcfun "last" :MaybeValue (:Doremir.List.List))

(defcfun "take" :Doremir.List.List (:int :Doremir.List.List))

(defcfun "drop" :Doremir.List.List (:int :Doremir.List.List))

(defcfun "isElem" :bool (:Doremir.List.Value :Doremir.List.List))

(defcfun "reverse" :Doremir.List.List (:Doremir.List.List))

(defcfun "sort" :Doremir.List.List (:Doremir.List.List))

(defcfun "sep" :Doremir.List.List (:Doremir.List.Value :Doremir.List.List))

(defcfun "concat" :Doremir.List.List (:Doremir.List.List))

(defcfun "concatSep" :Doremir.List.List (:Doremir.List.List))

(defctype Pred :long)

(defcfun "find" :MaybeValue (:Doremir.List.Pred :Doremir.List.List))

(defcfun "filter" :Doremir.List.List (:Doremir.List.Pred :Doremir.List.List))

(defctype UnaryFunc :long)

(defctype BinaryFunc :long)

(defcfun "map" :Doremir.List.List (:Doremir.List.List :Func))

(defcfun "foldl" :Doremir.List.List (:Doremir.List.List :Doremir.List.BinaryFunc :Doremir.List.Value))

(defcfun "and" :bool (:Doremir.List.List))

(defcfun "or" :bool (:Doremir.List.List))

(defcfun "any" :bool (:Doremir.List.Pred :Doremir.List.List))

(defcfun "all" :bool (:Doremir.List.Pred :Doremir.List.List))

(defcfun "sum" :Doremir.List.Value (:Num :Doremir.List.List))

(defcfun "product" :Doremir.List.Value (:Num :Doremir.List.List))

(defcfun "maximum" :Doremir.List.Value (:Doremir.List.List))

(defcfun "minimum" :Doremir.List.Value (:Doremir.List.List))

(defcfun "consDestroy" :Doremir.List.List (:Doremir.List.Value :Doremir.List.List))

(defcfun "snocDestroy" :Doremir.List.List (:Doremir.List.Value :Doremir.List.List))

(defcfun "reverseDestroy" :Doremir.List.List (:Doremir.List.List))

(defcfun "sortDestroy" :Doremir.List.List (:Doremir.List.List))

(defcfun "mapDestroy" :Doremir.List.List (:Doremir.List.List :Func))