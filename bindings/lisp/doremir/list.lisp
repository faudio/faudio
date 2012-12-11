(defctype Doremir.List :pointer)

(defctype Doremir.List.Value :pointer)

(defcfun "Doremir.List.empty" :Doremir.List ())

(defcfun "Doremir.List.cons" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.snoc" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.destroy" :void (:Doremir.List))

(defcfun "Doremir.List.copy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.swap" :void (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.isEmpty" :boolean (:Doremir.List))

(defcfun "Doremir.List.lenght" :int (:Doremir.List))

(defcfun "Doremir.List.take" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.drop" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.isElem" :boolean (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.reverse" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sort" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sep" :Doremir.List (:Doremir.List.Value :Doremir.List))

(defcfun "Doremir.List.concat" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.concatSep" :Doremir.List (:Doremir.List))

(defctype Doremir.List.Pred (:pointer (:pointer :void)))

(defcfun "Doremir.List.filter" :Doremir.List (:Doremir.List.Pred :Doremir.List))

(defcfun "Doremir.List.and" :boolean (:Doremir.List))

(defcfun "Doremir.List.or" :boolean (:Doremir.List))

(defcfun "Doremir.List.any" :boolean (:Doremir.List.Pred :Doremir.List))

(defcfun "Doremir.List.all" :boolean (:Doremir.List.Pred :Doremir.List))

(defctype Doremir.List.Unary (:pointer (:pointer :void)))

(defctype Doremir.List.Binary (:pointer (:pointer :void)))

(defcfun "Doremir.List.map" :Doremir.List (:Doremir.List :Doremir.List.Unary))

(defcfun "Doremir.List.foldl" :Doremir.List (:Doremir.List :Doremir.List.Binary :Doremir.List.Value))

(defctype Doremir.List.Num (:pointer :void))

(defcfun "Doremir.List.maximum" :Doremir.List.Value (:Doremir.List))

(defcfun "Doremir.List.minimum" :Doremir.List.Value (:Doremir.List))