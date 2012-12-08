(defctype Status (:pointer :void))

(defcfun "statusType" :int (:int))

(defcfun "statusChannel" :int (:int))

(defcfun "statusIsSysex" :bool (:int))

(defctype SimpleMessage (:pointer :void))

(defctype SysexMessage (:pointer :void))

(defctype MessageTag (:pointer :void))

(defctype Message (:pointer :void))