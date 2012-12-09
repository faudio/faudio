(defctype Doremir.FileDevice :pointer)

(defctype Doremir.Audio.Device.FileDevice.Stream :pointer)

(defcfun "Doremir.Audio.Device.FileDevice.create" :Doremir.FileDevice (:Doremir.String.FilePath :Doremir.String.FilePath))

(defcfun "Doremir.Audio.Device.FileDevice.destroy" :void (:Doremir.FileDevice))

(defcfun "Doremir.Audio.Device.FileDevice.closeStream" :void (:Doremir.Audio.Device.FileDevice.Stream))