(defctype Doremir.Buffer :pointer)

(defcfun "Doremir.AudioEngine.Device.Buffer.create" :Doremir.Buffer (:size))

(defcfun "Doremir.AudioEngine.Device.Buffer.destroy" :void (:Doremir.Buffer))