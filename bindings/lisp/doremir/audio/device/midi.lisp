(defctype Doremir.Audio.Device.Midi.Session :pointer)

(defctype Doremir.Midi :pointer)

(defctype Doremir.Audio.Device.Midi.Stream :pointer)

(defcfun "Doremir.Audio.Device.Midi.beginSession" :Doremir.Audio.Device.Midi.Session ())

(defcfun "Doremir.Audio.Device.Midi.endSession" :void (:Doremir.Audio.Device.Midi.Session))

(defcfun "Doremir.Audio.Device.Midi.devices" :Doremir.List (:Doremir.Audio.Device.Midi.Session))

(defcfun "Doremir.Audio.Device.Midi.standard" :Doremir.Pair (:Doremir.Audio.Device.Midi.Session))

(defcfun "Doremir.Audio.Device.Midi.closeStream" :void (:Doremir.Audio.Device.Midi.Stream))