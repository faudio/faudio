(defctype Doremir.Buffer :pointer)

(defcfun "Doremir.Buffer.create" :Doremir.Buffer (:size))

(defcfun "Doremir.Buffer.copy" :Doremir.Buffer (:Doremir.Buffer))

(defcfun "Doremir.Buffer.resize" :Doremir.Buffer (:size :Doremir.Buffer))

(defcfun "Doremir.Buffer.destroy" :void (:Doremir.Buffer))

(defcfun "Doremir.Buffer.size" :size (:Doremir.Buffer))

(defcfun "Doremir.Buffer.peek" :uint8 (:Doremir.Buffer :int))

(defcfun "Doremir.Buffer.poke" :void (:Doremir.Buffer :int :uint8))