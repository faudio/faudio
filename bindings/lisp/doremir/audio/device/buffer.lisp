(defctype BufferDevice :long)

(defcfun "create" :Doremir.Audio.Device.BufferDevice.BufferDevice (:long :Type))

(defcfun "destroy" :void (:Doremir.Audio.Device.BufferDevice.BufferDevice))