(defctype Doremir.BufferDevice :pointer)

(defcfun "Doremir.Audio.Device.BufferDevice.create" :Doremir.BufferDevice (:size :Doremir.Audio.Type))

(defcfun "Doremir.Audio.Device.BufferDevice.destroy" :void (:Doremir.BufferDevice))