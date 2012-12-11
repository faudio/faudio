(defctype Doremir.String (:pointer :char))

(defctype Doremir.String.Utf8 (:pointer :uint8))

(defctype Doremir.String.Utf16 (:pointer :uint16))

(defctype Doremir.String.Utf32 (:pointer :uint32))

(defctype Doremir.String.FilePath :Doremir.String)