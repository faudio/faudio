(defctype Doremir.Device.Audio.Session :pointer)

(defctype Doremir.Device.Audio.Stream :pointer)

(defctype Doremir.Device.Audio :pointer)

(defcfun "Doremir.Device.Audio.beginSession" :Doremir.Device.Audio.Session ())

(defcfun "Doremir.Device.Audio.endSession" :void (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.devices" :Doremir.List (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.standard" :Doremir.Pair (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.standardInput" :Doremir.Device.Audio (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.standardOutput" :Doremir.Device.Audio (:Doremir.Device.Audio.Session))

(defcfun "Doremir.Device.Audio.openStream" :Doremir.Device.Audio.Stream (:Doremir.Device.Audio :Doremir.Processor :Doremir.Device.Audio))

(defcfun "Doremir.Device.Audio.closeStream" :void (:Doremir.Device.Audio.Stream))