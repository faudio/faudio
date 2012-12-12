(defctype Doremir.Device.Buffer :pointer)

(defcfun "Doremir.Device.Buffer.create" :Doremir.Device.Buffer (:size))

(defcfun "Doremir.Device.Buffer.destroy" :void (:Doremir.Device.Buffer))