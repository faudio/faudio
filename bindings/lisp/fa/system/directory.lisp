(in-package :faudio)
(defcfun (system-directory-home "fa_system_directory_home") string)
(defcfun (system-directory-current "fa_system_directory_current") string)
(defcfun (system-directory-create "fa_system_directory_create") :void (a string))
(defcfun (system-directory-remove "fa_system_directory_remove") :void (a string))
(defcfun (system-directory-read-file "fa_system_directory_read_file") string (a string))
(defcfun (system-directory-write-file "fa_system_directory_write_file") :void (a string) (b string))
(defcfun (system-directory-append-file "fa_system_directory_append_file") :void (a string) (b string))

