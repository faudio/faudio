(defctype Doremir.Device.File :pointer)

(defctype Doremir.Device.File.Stream :pointer)

(defcfun "Doremir.Device.File.create" :Doremir.Device.File (:Doremir.String.FilePath :Doremir.String.FilePath))

(defcfun "Doremir.Device.File.destroy" :void (:Doremir.Device.File))

(defcfun "Doremir.Device.File.closeStream" :void (:Doremir.Device.File.Stream))