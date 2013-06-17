(in-package :cl-user)
(defcfun (string-list-convert "fae_string_list_convert") list (a string))
(defcfun (string-list-unconvert "fae_string_list_unconvert") string (a list))