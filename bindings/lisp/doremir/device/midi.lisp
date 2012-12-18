(defctype Doremir.Device.Midi.Session :pointer)

(defctype Doremir.Device.Midi.Stream :pointer)

(defctype Doremir.Device.Midi :pointer)

(defcfun "Doremir.Device.Midi.beginSession" :Doremir.Device.Midi.Session ())

(defcfun "Doremir.Device.Midi.endSession" :void (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.devices" :Doremir.List (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.standard" :Doremir.Pair (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.standardInput" :Doremir.Device.Midi (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.standardOutput" :Doremir.Device.Midi (:Doremir.Device.Midi.Session))

(defcfun "Doremir.Device.Midi.openStream" :Doremir.Device.Midi.Stream (:Doremir.Device.Midi :Doremir.Processor :Doremir.Device.Midi))

(defcfun "Doremir.Device.Midi.closeStream" :void (:Doremir.Device.Midi.Stream))