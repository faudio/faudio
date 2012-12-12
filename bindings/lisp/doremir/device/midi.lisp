(defctype Doremir.Device.Midi.Session :pointer)

(defctype Doremir.Device.Midi :pointer)

(defctype Doremir.Device.Midi.Stream :pointer)

(defcfun "Doremir.Device.Midi.beginSession" :Doremir.Device.Midi.Session ())

(defcfun "Doremir.Device.Midi.endSession" :void (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.devices" :Doremir.List (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.standard" :Doremir.Pair (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.closeStream" :void (:Doremir.Device.Midi.Stream))