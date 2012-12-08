(defctype Session :long)

(defctype MidiDevice :long)

(defctype Stream :long)

(defctype MidiDeviceList :List)

(defcfun "beginSession" :Doremir.Audio.Device.MidiDevice.Session ())

(defcfun "endSession" :void (:Doremir.Audio.Device.MidiDevice.Session))

(defcfun "devices" :Doremir.Audio.Device.MidiDevice.MidiDeviceList (:Doremir.Audio.Device.MidiDevice.Session))

(defcfun "standard" :MidiDevicePair (:Doremir.Audio.Device.MidiDevice.Session))

(defcfun "openStream" :Doremir.Audio.Device.MidiDevice.Stream (:Doremir.Audio.Device.MidiDevice.MidiDevice :Proc))

(defcfun "closeStream" :void (:Doremir.Audio.Device.MidiDevice.Stream))