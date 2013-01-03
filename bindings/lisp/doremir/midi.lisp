(defctype Doremir.Midi.Status (:pointer :void))

(defctype Doremir.Midi.Channel :int)

(defctype Doremir.Midi.Data :int)

(defctype Doremir.Midi :pointer)

(defcfun "Doremir.Midi.createSimple" :Doremir.Midi (:Doremir.Midi.Status :int :int))

(defcfun "Doremir.Midi.createSysex" :Doremir.Midi (:Doremir.Buffer))

(defcfun "Doremir.Midi.copy" :Doremir.Midi (:Doremir.Midi))

(defcfun "Doremir.Midi.destroy" :void (:Doremir.Midi))

(defcfun "Doremir.Midi.status" :Doremir.Midi.Status (:Doremir.Midi))

(defcfun "Doremir.Midi.channel" :Doremir.Midi.Channel (:Doremir.Midi))

(defcfun "Doremir.Midi.isSimple" :boolean (:Doremir.Midi))

(defcfun "Doremir.Midi.simpleData" :Doremir.Pair (:Doremir.Midi))

(defcfun "Doremir.Midi.isSysex" :boolean (:Doremir.Midi))

(defcfun "Doremir.Midi.sysexData" :Doremir.Buffer (:Doremir.Midi))