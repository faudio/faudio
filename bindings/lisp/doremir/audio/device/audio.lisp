(defctype AudioDevice :long)

(defctype Session :long)

(defctype Stream :long)

(defctype AudioDeviceList :List)

(defcfun "beginSession" :Doremir.Audio.Device.AudioDevice.Session ())

(defcfun "endSession" :void (:Doremir.Audio.Device.AudioDevice.Session))

(defcfun "devices" :Doremir.Audio.Device.AudioDevice.AudioDeviceList (:Doremir.Audio.Device.AudioDevice.Session))

(defcfun "standard" :AudioDevicePair (:Doremir.Audio.Device.AudioDevice.Session))

(defcfun "openStream" :Doremir.Audio.Device.AudioDevice.Stream (:Doremir.Audio.Device.AudioDevice.AudioDevice :Proc))

(defcfun "closeStream" :void (:Doremir.Audio.Device.AudioDevice.Stream))