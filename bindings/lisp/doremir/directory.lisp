(in-package :audio-engine)
(defcfun (directory-home "doremir_directory_home") string-file-path)
(defcfun (directory-current "doremir_directory_current") string-file-path)