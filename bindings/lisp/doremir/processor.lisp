(defctype Doremir.Processor.Info (:pointer :void))

(defcfun "Doremir.Processor.infoDefault" :void ((:pointer :Doremir.Processor.Info)))

(defctype Doremir.Processor.Samples :Doremir.Buffer)

(defctype Doremir.Processor (:pointer :void))

(defctype Doremir.Processor.Any :Doremir.Ptr)

(defcfun "Doremir.Processor.unary" :Doremir.Processor.Any (:Doremir.Type :Doremir.Unary))

(defcfun "Doremir.Processor.binary" :Doremir.Processor.Any (:Doremir.Type :Doremir.Binary))

(defcfun "Doremir.Processor.ternary" :Doremir.Processor.Any (:Doremir.Type :Doremir.Ternary))

(defcfun "Doremir.Processor.identity" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.constant" :Doremir.Processor.Any (:Doremir.Type :Doremir.Ptr))

(defcfun "Doremir.Processor.delay" :Doremir.Processor.Any (:Doremir.Type :size))

(defcfun "Doremir.Processor.split" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.sequence" :Doremir.Processor.Any (:Doremir.Processor.Any :Doremir.Processor.Any))

(defcfun "Doremir.Processor.parallel" :Doremir.Processor.Any (:Doremir.Processor.Any :Doremir.Processor.Any))

(defcfun "Doremir.Processor.loop" :Doremir.Processor.Any (:Doremir.Processor.Any))

(defcfun "Doremir.Processor.cos" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.sin" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.tan" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.acos" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.asin" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.atan" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.add" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.subtract" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.multiply" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.divide" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.modulo" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.absolute" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.and" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.or" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.not" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.bitAnd" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.bitOr" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.bitNot" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.bitXor" :Doremir.Processor.Any (:Doremir.Type))