(defctype Doremir.AudioEngine.Device.Midi.Session :pointer)

(defctype Doremir.Midi :pointer)

(defctype Doremir.AudioEngine.Device.Midi.Stream :pointer)

(defcfun "Doremir.AudioEngine.Device.Midi.beginSession" :Doremir.AudioEngine.Device.Midi.Session ())

(defcfun "Doremir.AudioEngine.Device.Midi.endSession" :void (:Doremir.AudioEngine.Device.Midi.Session))

(defcfun "Doremir.AudioEngine.Device.Midi.devices" :Doremir.List (:Doremir.AudioEngine.Device.Midi.Session))

(defcfun "Doremir.AudioEngine.Device.Midi.standard" :Doremir.Pair (:Doremir.AudioEngine.Device.Midi.Session))

(defcfun "Doremir.AudioEngine.Device.Midi.closeStream" :void (:Doremir.AudioEngine.Device.Midi.Stream))