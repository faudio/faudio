(in-package :audio-engine)
(defctype dynamic-type-repr :int)
(defctype dynamic (:pointer :void))
(defcfun (dynamic-get-type "doremir_dynamic_get_type") dynamic-type-repr (a ptr))