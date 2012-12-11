(defctype Doremir.File :pointer)

(defctype Doremir.Audio.Device.File.Stream :pointer)

(defcfun "Doremir.Audio.Device.File.create" :Doremir.File (:Doremir.String.FilePath :Doremir.String.FilePath))

(defcfun "Doremir.Audio.Device.File.destroy" :void (:Doremir.File))

(defcfun "Doremir.Audio.Device.File.closeStream" :void (:Doremir.Audio.Device.File.Stream))