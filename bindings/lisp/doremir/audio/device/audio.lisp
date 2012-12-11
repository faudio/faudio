(defctype Doremir.Audio.Device.Audio.Session :pointer)

(defctype Doremir.Audio :pointer)

(defctype Doremir.Audio.Device.Audio.Stream :pointer)

(defcfun "Doremir.Audio.Device.Audio.beginSession" :Doremir.Audio.Device.Audio.Session ())

(defcfun "Doremir.Audio.Device.Audio.endSession" :void (:Doremir.Audio.Device.Audio.Session))

(defcfun "Doremir.Audio.Device.Audio.devices" :Doremir.List (:Doremir.Audio.Device.Audio.Session))

(defcfun "Doremir.Audio.Device.Audio.standard" :Doremir.Pair (:Doremir.Audio.Device.Audio.Session))

(defcfun "Doremir.Audio.Device.Audio.closeStream" :void (:Doremir.Audio.Device.Audio.Stream))