(defctype Func :long)

(defctype Proc :long)

(defcfun "lift" :Doremir.Audio.Processor.Unary.Proc (:Doremir.Audio.Processor.Unary.Func))