(defctype Doremir.Buffered :pointer)

(defcfun "Doremir.AudioEngine.Device.Buffered.create" :Doremir.Buffered (:size))

(defcfun "Doremir.AudioEngine.Device.Buffered.destroy" :void (:Doremir.Buffered))