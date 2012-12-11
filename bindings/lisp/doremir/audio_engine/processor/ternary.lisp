(defctype Doremir.Audio.Processor.Ternary.Func :pointer)

(defctype Doremir.Audio.Processor.Ternary.Proc :pointer)

(defcfun "Doremir.Audio.Processor.Ternary.lift" :Doremir.Audio.Processor.Ternary.Proc (:Doremir.Audio.Processor.Ternary.Func))