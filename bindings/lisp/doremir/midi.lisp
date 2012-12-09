(defctype Doremir.Midi.Status (:pointer :void))

(defcfun "Doremir.Midi.statusType" :int (:int))

(defcfun "Doremir.Midi.statusChannel" :int (:int))

(defcfun "Doremir.Midi.statusIsSysex" :boolean (:int))

(defctype Doremir.Midi.SimpleMessage (:pointer :void))

(defctype Doremir.Midi.SysexMessage (:pointer :void))

(defctype Doremir.Midi.MessageTag (:pointer :void))

(defctype Doremir.Midi.Message (:pointer :void))