(defctype Doremir.AudioEngine.Device.Audio.Session :pointer)

(defctype Doremir.Audio :pointer)

(defctype Doremir.AudioEngine.Device.Audio.Stream :pointer)

(defcfun "Doremir.AudioEngine.Device.Audio.beginSession" :Doremir.AudioEngine.Device.Audio.Session ())

(defcfun "Doremir.AudioEngine.Device.Audio.endSession" :void (:Doremir.AudioEngine.Device.Audio.Session))

(defcfun "Doremir.AudioEngine.Device.Audio.devices" :Doremir.List (:Doremir.AudioEngine.Device.Audio.Session))

(defcfun "Doremir.AudioEngine.Device.Audio.standard" :Doremir.Pair (:Doremir.AudioEngine.Device.Audio.Session))

(defcfun "Doremir.AudioEngine.Device.Audio.closeStream" :void (:Doremir.AudioEngine.Device.Audio.Stream))