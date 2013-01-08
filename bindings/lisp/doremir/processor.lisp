(defctype Doremir.Processor.Info (:pointer :void))

(defctype Doremir.Processor.Samples :Doremir.Buffer)

(defctype Doremir.Processor (:pointer :void))

(defctype Doremir.Processor.Any :Doremir.Ptr)

(defcfun "Doremir.Processor.unary" :Doremir.Processor.Any (:Doremir.Type :Doremir.Type :Doremir.Unary))

(defcfun "Doremir.Processor.binary" :Doremir.Processor.Any (:Doremir.Type :Doremir.Type :Doremir.Type :Doremir.Binary))

(defcfun "Doremir.Processor.ternary" :Doremir.Processor.Any (:Doremir.Type :Doremir.Type :Doremir.Type :Doremir.Type :Doremir.Ternary))

(defcfun "Doremir.Processor.identity" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.constant" :Doremir.Processor.Any (:Doremir.Type :Doremir.Type :Doremir.Ptr))

(defcfun "Doremir.Processor.delay" :Doremir.Processor.Any (:Doremir.Type :size))

(defcfun "Doremir.Processor.split" :Doremir.Processor.Any (:Doremir.Type))

(defcfun "Doremir.Processor.seq" :Doremir.Processor.Any (:Doremir.Processor.Any :Doremir.Processor.Any))

(defcfun "Doremir.Processor.par" :Doremir.Processor.Any (:Doremir.Processor.Any :Doremir.Processor.Any))

(defcfun "Doremir.Processor.loop" :Doremir.Processor.Any (:Doremir.Processor.Any))