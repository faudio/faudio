(in-package :audio-engine)
(defcfun (string-list-convert "doremir_string_list_convert") list (a string))
(defcfun (string-list-unconvert "doremir_string_list_unconvert") string (a list))