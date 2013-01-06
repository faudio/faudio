(defctype Doremir.Signal :pointer)

(defcfun "Doremir.Signal.typeOf" :Doremir.Type (:Doremir.Signal))

(defcfun "Doremir.Signal.applyUnary" :Doremir.Signal (:Doremir.Processor.Any :Doremir.Signal))

(defcfun "Doremir.Signal.applyBinary" :Doremir.Signal (:Doremir.Processor.Any :Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.applyTernary" :Doremir.Signal (:Doremir.Processor.Any :Doremir.Signal :Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.const" :Doremir.Signal (:Doremir.Ptr :Doremir.Signal))

(defcfun "Doremir.Signal.delay" :Doremir.Signal (:size :Doremir.Signal))

(defcfun "Doremir.Signal.split" :Doremir.Pair (:Doremir.Signal))

(defcfun "Doremir.Signal.cos" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.sin" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.tan" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.acos" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.asin" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.atan" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.add" :Doremir.Signal (:Doremir.Signal))

(defcfun "Doremir.Signal.subtract" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.multiply" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.divide" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.modulo" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.absolute" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.and" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.or" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.not" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.bitAnd" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.bitOr" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.bitNot" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))

(defcfun "Doremir.Signal.bitXor" :Doremir.Signal (:Doremir.Signal :Doremir.Signal))