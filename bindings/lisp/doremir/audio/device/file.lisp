(defctype FileDevice :long)

(defcfun "create" :Doremir.Audio.Device.FileDevice.FileDevice (:FilePath :FilePath))

(defcfun "destroy" :void (:Doremir.Audio.Device.FileDevice.FileDevice))

(defcfun "openStream" :Stream (:Doremir.Audio.Device.FileDevice.FileDevice :Proc))

(defcfun "closeStream" :void (:Stream))