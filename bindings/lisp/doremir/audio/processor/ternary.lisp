(defctype Func :long)

(defctype Proc :long)

(defcfun "lift" :Doremir.Audio.Processor.Ternary.Proc (:Doremir.Audio.Processor.Ternary.Func))