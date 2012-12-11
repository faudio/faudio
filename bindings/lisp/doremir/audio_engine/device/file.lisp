(defctype Doremir.Audio.Engine.Device.File :pointer)

(defctype Doremir.AudioEngine.Device.File.Stream :pointer)

(defcfun "Doremir.AudioEngine.Device.File.create" :Doremir.Audio.Engine.Device.File (:Doremir.String.FilePath :Doremir.String.FilePath))

(defcfun "Doremir.AudioEngine.Device.File.destroy" :void (:Doremir.Audio.Engine.Device.File))

(defcfun "Doremir.AudioEngine.Device.File.closeStream" :void (:Doremir.AudioEngine.Device.File.Stream))