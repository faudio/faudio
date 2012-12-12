(defctype Doremir.Device.Audio.Session :pointer)

(defctype Doremir.Device.Audio.Device :pointer)

(defctype Doremir.Device.Audio.Stream :pointer)

(defcfun "Doremir.Device.Audio.beginSession" :Doremir.Device.Audio.Session ())

(defcfun "Doremir.Device.Audio.endSession" :void (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.devices" :Doremir.List (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.standard" :Doremir.Pair (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.closeStream" :void (:Doremir.Device.Audio.Stream))