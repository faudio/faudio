(defctype Doremir.Audio.Processor.Binary.Func :pointer)

(defctype Doremir.Audio.Processor.Binary.Proc :pointer)

(defcfun "Doremir.Audio.Processor.Binary.lift" :Doremir.Audio.Processor.Binary.Proc (:Doremir.Audio.Processor.Binary.Func))