(defctype Doremir.Audio.Device.MidiDevice.Session :pointer)

(defctype Doremir.MidiDevice :pointer)

(defctype Doremir.Audio.Device.MidiDevice.Stream :pointer)

(defcfun "Doremir.Audio.Device.MidiDevice.beginSession" :Doremir.Audio.Device.MidiDevice.Session ())

(defcfun "Doremir.Audio.Device.MidiDevice.endSession" :void (:Doremir.Audio.Device.MidiDevice.Session))

(defcfun "Doremir.Audio.Device.MidiDevice.devices" :Doremir.List (:Doremir.Audio.Device.MidiDevice.Session))

(defcfun "Doremir.Audio.Device.MidiDevice.standard" :Doremir.Pair (:Doremir.Audio.Device.MidiDevice.Session))

(defcfun "Doremir.Audio.Device.MidiDevice.closeStream" :void (:Doremir.Audio.Device.MidiDevice.Stream))