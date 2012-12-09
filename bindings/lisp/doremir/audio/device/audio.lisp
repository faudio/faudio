(defctype Doremir.Audio.Device.AudioDevice.Session :pointer)

(defctype Doremir.AudioDevice :pointer)

(defctype Doremir.Audio.Device.AudioDevice.Stream :pointer)

(defcfun "Doremir.Audio.Device.AudioDevice.beginSession" :Doremir.Audio.Device.AudioDevice.Session ())

(defcfun "Doremir.Audio.Device.AudioDevice.endSession" :void (:Doremir.Audio.Device.AudioDevice.Session))

(defcfun "Doremir.Audio.Device.AudioDevice.devices" :Doremir.List (:Doremir.Audio.Device.AudioDevice.Session))

(defcfun "Doremir.Audio.Device.AudioDevice.standard" :Doremir.Pair (:Doremir.Audio.Device.AudioDevice.Session))

(defcfun "Doremir.Audio.Device.AudioDevice.closeStream" :void (:Doremir.Audio.Device.AudioDevice.Stream))