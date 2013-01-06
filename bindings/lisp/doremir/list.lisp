(defctype Doremir.List :pointer)

(defcfun "Doremir.List.empty" :Doremir.List ())

(defcfun "Doremir.List.single" :Doremir.List (:Doremir.Ptr))

(defcfun "Doremir.List.cons" :Doremir.List (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.append" :Doremir.List (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.dappend" :Doremir.List (:Doremir.List :Doremir.List))

(defcfun "Doremir.List.copy" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.destroy" :void (:Doremir.List))

(defcfun "Doremir.List.isEmpty" :boolean (:Doremir.List))

(defcfun "Doremir.List.isSingle" :boolean (:Doremir.List))

(defcfun "Doremir.List.length" :int (:Doremir.List))

(defcfun "Doremir.List.head" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.tail" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.dtail" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.init" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.dinit" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.last" :Doremir.Ptr (:Doremir.List))

(defcfun "Doremir.List.take" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.dtake" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.drop" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.ddrop" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.range" :Doremir.List (:int :int :Doremir.List))

(defcfun "Doremir.List.index" :Doremir.Ptr (:int :Doremir.List))

(defcfun "Doremir.List.insert" :Doremir.List (:int :Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.remove" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.removeRange" :Doremir.List (:int :int :Doremir.List))

(defcfun "Doremir.List.dremove" :Doremir.List (:int :Doremir.List))

(defcfun "Doremir.List.dremoveRange" :Doremir.List (:int :int :Doremir.List))

(defcfun "Doremir.List.has" :boolean (:Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.find" :Doremir.Ptr (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.findIndex" :int (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.map" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.dmap" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.concatMap" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.dconcatMap" :Doremir.List (:Doremir.Unary :Doremir.List))

(defcfun "Doremir.List.filter" :Doremir.List (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.dfilter" :Doremir.List (:Doremir.Pred :Doremir.List))

(defcfun "Doremir.List.foldLeft" :Doremir.Ptr (:Doremir.Binary :Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.dfoldLeft" :Doremir.Ptr (:Doremir.Binary :Doremir.Ptr :Doremir.List))

(defcfun "Doremir.List.concat" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.sort" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.dsort" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.reverse" :Doremir.List (:Doremir.List))

(defcfun "Doremir.List.dreverse" :Doremir.List (:Doremir.List))