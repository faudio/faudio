(defctype Doremir.Audio.Processor.Unary.Func :pointer)

(defctype Doremir.Audio.Processor.Unary.Proc :pointer)

(defcfun "Doremir.Audio.Processor.Unary.lift" :Doremir.Audio.Processor.Unary.Proc (:Doremir.Audio.Processor.Unary.Func))