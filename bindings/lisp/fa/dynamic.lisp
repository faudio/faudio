(in-package :faudio)
(defctype dynamic-type-repr :int)
(defctype dynamic (:pointer :void))
(defcfun (dynamic-check "fa_dynamic_check") :boolean (a ptr))
(defcfun (dynamic-get-type "fa_dynamic_get_type") dynamic-type-repr (a ptr))

