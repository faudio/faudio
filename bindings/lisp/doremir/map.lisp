(defctype Map :long)

(defctype Key :long)

(defctype Value :long)

(defcfun "empty" :Doremir.Map.Map ())

(defcfun "single" :Doremir.Map.Map (:Doremir.Map.Key :Doremir.Map.Value))

(defcfun "append" :Doremir.Map.Map (:Doremir.Map.Map :Doremir.Map.Map))

(defcfun "destroy" :void (:Doremir.Map.Map))

(defcfun "copy" :Doremir.Map.Map (:Doremir.Map.Map))

(defcfun "swap" :void (:Doremir.Map.Map :Doremir.Map.Map))

(defcfun "key" :bool (:Doremir.Map.Map :Doremir.Map.Key))

(defcfun "elem" :bool (:Doremir.Map.Map :Doremir.Map.Value))

(defcfun "entry" :bool (:Doremir.Map.Map :Pair))

(defcfun "submapOf" :bool (:Doremir.Map.Map :Doremir.Map.Map))

(defcfun "properSubmapOf" :bool (:Doremir.Map.Map :Doremir.Map.Map))

(defcfun "add" :Doremir.Map.Map (:Doremir.Map.Map :Doremir.Map.Key :Doremir.Map.Value))

(defcfun "remove" :void (:Doremir.Map.Map :Doremir.Map.Key :Doremir.Map.Value))

(defcfun "addDest" :Doremir.Map.Map (:Doremir.Map.Map :Doremir.Map.Key :Doremir.Map.Value))

(defcfun "removeDest" :void (:Doremir.Map.Map :Doremir.Map.Key :Doremir.Map.Value))