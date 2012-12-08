(defctype Func :long)

(defctype Proc :long)

(defcfun "lift" :Doremir.Audio.Processor.Binary.Proc (:Doremir.Audio.Processor.Binary.Func))