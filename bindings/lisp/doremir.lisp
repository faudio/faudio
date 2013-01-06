(defctype Doremir.Ptr (:pointer :void))

(defctype Doremir.Nullary (:pointer (:pointer :void)))

(defctype Doremir.Unary (:pointer (:pointer :void)))

(defctype Doremir.Binary (:pointer (:pointer :void)))

(defctype Doremir.Ternary (:pointer (:pointer :void)))

(defctype Doremir.Pred (:pointer (:pointer :void)))

(defctype Doremir.Closure (:pointer :void))

(defctype Doremir.Char8 :char)

(defctype Doremir.Char16 :uint16)

(defctype Doremir.Char32 :uint32)

(defcfun "Doremir.equal" :boolean (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.notEqual" :boolean (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.lessThan" :boolean (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.greaterThan" :boolean (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.lessThanEqual" :boolean (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.greaterThanEqual" :boolean (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.min" :Doremir.Ptr (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.max" :Doremir.Ptr (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.add" :Doremir.Ptr (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.subtract" :Doremir.Ptr (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.multiply" :Doremir.Ptr (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.divide" :Doremir.Ptr (:Doremir.Ptr :Doremir.Ptr))

(defcfun "Doremir.absolute" :Doremir.Ptr (:Doremir.Ptr))

(defcfun "Doremir.copy" :Doremir.Ptr (:Doremir.Ptr))

(defcfun "Doremir.move" :Doremir.Ptr (:Doremir.Ptr))

(defcfun "Doremir.destroy" :void (:Doremir.Ptr))

(defcfun "Doremir.print" :void ((:pointer :char) :Doremir.Ptr))

(defctype Doremir.Equal (:pointer :void))

(defctype Doremir.Order (:pointer :void))

(defctype Doremir.Copy (:pointer :void))

(defctype Doremir.Destroy (:pointer :void))

(defctype Doremir.Number (:pointer :void))

(defctype Doremir.TypeRepr (:pointer :void))

(defctype Doremir.Dynamic (:pointer :void))

(defcfun "Doremir.typeStr" (:pointer :char) (:Doremir.Ptr))

(defcfun "Doremir.toBool" :boolean (:Doremir.Ptr))

(defcfun "Doremir.toInt8" :int8 (:Doremir.Ptr))

(defcfun "Doremir.toInt16" :int16 (:Doremir.Ptr))

(defcfun "Doremir.toInt32" :int32 (:Doremir.Ptr))

(defcfun "Doremir.toInt64" :int64 (:Doremir.Ptr))

(defcfun "Doremir.toFloat" :float (:Doremir.Ptr))

(defcfun "Doremir.toDouble" :double (:Doremir.Ptr))

(defcfun "Doremir.fromBool" :Doremir.Ptr (:boolean))

(defcfun "Doremir.fromInt8" :Doremir.Ptr (:int8))

(defcfun "Doremir.fromInt16" :Doremir.Ptr (:int16))

(defcfun "Doremir.fromInt32" :Doremir.Ptr (:int32))

(defcfun "Doremir.fromInt64" :Doremir.Ptr (:int64))

(defcfun "Doremir.fromFloat" :Doremir.Ptr (:float))

(defcfun "Doremir.fromDouble" :Doremir.Ptr (:double))

(defctype Doremir.Id :int64)

(defctype Doremir.Impl (:pointer (:pointer :void)))

(defcfun "Doremir.interface" :Doremir.Ptr (:Doremir.Id :Doremir.Ptr))