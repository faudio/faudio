(defctype Doremir.String :pointer)

(defctype Doremir.String.FilePath :Doremir.String)

(defctype Doremir.String.Utf8 (:pointer :Doremir.Char8))

(defctype Doremir.String.Utf16 (:pointer :Doremir.Char16))

(defctype Doremir.String.Utf32 (:pointer :Doremir.Char32))

(defcfun "Doremir.String.empty" :Doremir.String ())

(defcfun "Doremir.String.single" :Doremir.String (:Doremir.Char16))

(defcfun "Doremir.String.copy" :Doremir.String (:Doremir.String))

(defcfun "Doremir.String.append" :Doremir.String (:Doremir.String :Doremir.String))

(defcfun "Doremir.String.dappend" :Doremir.String (:Doremir.String :Doremir.String))

(defcfun "Doremir.String.destroy" :void (:Doremir.String))

(defcfun "Doremir.String.length" :int (:Doremir.String))

(defcfun "Doremir.String.charAt" :Doremir.Char16 (:int :Doremir.String))

(defctype Doremir.String.Show (:pointer :void))

(defcfun "Doremir.String.show" :Doremir.String (:Doremir.Ptr))

(defcfun "Doremir.String.toUtf8" :Doremir.String.Utf8 (:Doremir.String))

(defcfun "Doremir.String.toUtf16" :Doremir.String.Utf16 (:Doremir.String))

(defcfun "Doremir.String.toUtf32" :Doremir.String.Utf32 (:Doremir.String))

(defcfun "Doremir.String.fromUtf8" :Doremir.String (:Doremir.String.Utf8))

(defcfun "Doremir.String.fromUtf16" :Doremir.String (:Doremir.String.Utf16))

(defcfun "Doremir.String.fromUtf32" :Doremir.String (:Doremir.String.Utf32))