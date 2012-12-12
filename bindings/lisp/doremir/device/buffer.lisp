(defctype Doremir.Device.Buffer :pointer)

(defctype Doremir.Device.Buffer.Stream :pointer)

(defcfun "Doremir.Device.Buffer.create" :Doremir.Device.Buffer (:size))

(defcfun "Doremir.Device.Buffer.destroy" :void (:Doremir.Device.Buffer))