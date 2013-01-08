(defctype Doremir.Type.Frames :size)

(defctype Doremir.Type.Simple (:pointer :void))

(defctype Doremir.Type.Struct (:pointer :void))

(defctype Doremir.Type (:pointer :Doremir.Type.Struct))

(defcfun "Doremir.Type.simple" :Doremir.Type (:Doremir.Type.Simple))

(defcfun "Doremir.Type.pair" :Doremir.Type (:Doremir.Type :Doremir.Type))

(defcfun "Doremir.Type.vector" :Doremir.Type (:Doremir.Type :size))

(defcfun "Doremir.Type.frame" :Doremir.Type (:Doremir.Type))

(defcfun "Doremir.Type.copy" :Doremir.Type (:Doremir.Type))

(defcfun "Doremir.Type.destroy" :void (:Doremir.Type))

(defcfun "Doremir.Type.isSimple" :boolean (:Doremir.Type))

(defcfun "Doremir.Type.isPair" :boolean (:Doremir.Type))

(defcfun "Doremir.Type.isVector" :boolean (:Doremir.Type))

(defcfun "Doremir.Type.isFrame" :boolean (:Doremir.Type))

(defcfun "Doremir.Type.sizeOf" :size (:Doremir.Type.Frames :Doremir.Type))

(defcfun "Doremir.Type.alignOf" :size (:Doremir.Type))