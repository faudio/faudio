(defctype Doremir.Ratio.Nom :int32)

(defctype Doremir.Ratio.Denom :uint32)

(defctype Doremir.Ratio (:pointer :void))

(defcfun "Doremir.Ratio.add" :Doremir.Ratio (:Doremir.Ratio :Doremir.Ratio))

(defcfun "Doremir.Ratio.subtract" :Doremir.Ratio (:Doremir.Ratio :Doremir.Ratio))

(defcfun "Doremir.Ratio.multiply" :Doremir.Ratio (:Doremir.Ratio :Doremir.Ratio))

(defcfun "Doremir.Ratio.divide" :Doremir.Ratio (:Doremir.Ratio :Doremir.Ratio))

(defcfun "Doremir.Ratio.remainder" :Doremir.Ratio (:Doremir.Ratio :Doremir.Ratio))

(defcfun "Doremir.Ratio.succ" :Doremir.Ratio (:Doremir.Ratio))

(defcfun "Doremir.Ratio.pred" :Doremir.Ratio (:Doremir.Ratio))

(defcfun "Doremir.Ratio.negate" :Doremir.Ratio (:Doremir.Ratio))

(defcfun "Doremir.Ratio.recip" :Doremir.Ratio (:Doremir.Ratio))