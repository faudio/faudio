(defctype Doremir.Raw :pointer)

(defcfun "Doremir.Audio.Device.Raw.create" :Doremir.Raw (:size :Doremir.Audio.Type))

(defcfun "Doremir.Audio.Device.Raw.destroy" :void (:Doremir.Raw))