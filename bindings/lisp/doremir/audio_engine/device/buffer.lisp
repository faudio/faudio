(defctype Doremir.Audio.Engine.Device.Buffer :pointer)

(defcfun "Doremir.AudioEngine.Device.Buffer.create" :Doremir.Audio.Engine.Device.Buffer (:size))

(defcfun "Doremir.AudioEngine.Device.Buffer.destroy" :void (:Doremir.Audio.Engine.Device.Buffer))