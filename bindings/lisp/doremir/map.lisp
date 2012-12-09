(defctype Doremir.Map :pointer)

(defctype Doremir.Map.Key :pointer)

(defctype Doremir.Map.Value :pointer)

(defcfun "Doremir.Map.empty" :Doremir.Map ())

(defcfun "Doremir.Map.single" :Doremir.Map (:Doremir.Map.Key :Doremir.Map.Value))

(defcfun "Doremir.Map.append" :Doremir.Map (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.destroy" :void (:Doremir.Map))

(defcfun "Doremir.Map.copy" :Doremir.Map (:Doremir.Map))

(defcfun "Doremir.Map.swap" :void (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.fromPair" :Doremir.Map (:Doremir.Pair))

(defcfun "Doremir.Map.key" :boolean (:Doremir.Map :Doremir.Map.Key))

(defcfun "Doremir.Map.elem" :boolean (:Doremir.Map :Doremir.Map.Value))

(defcfun "Doremir.Map.entry" :boolean (:Doremir.Map :Doremir.Pair))

(defcfun "Doremir.Map.submapOf" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.properSubmapOf" :boolean (:Doremir.Map :Doremir.Map))

(defcfun "Doremir.Map.add" :Doremir.Map (:Doremir.Map :Doremir.Map.Key :Doremir.Map.Value))

(defcfun "Doremir.Map.remove" :void (:Doremir.Map :Doremir.Map.Key :Doremir.Map.Value))

(defcfun "Doremir.Map.addDest" :Doremir.Map (:Doremir.Map :Doremir.Map.Key :Doremir.Map.Value))

(defcfun "Doremir.Map.removeDest" :void (:Doremir.Map :Doremir.Map.Key :Doremir.Map.Value))