module Fae.Signal where
 
data Signal
 
foreign import ccall unsafe "fae_signal_type_of" Fae.Signal.typeOf
        :: Fae.Signal -> Fae.Type
 
foreign import ccall unsafe "fae_signal_lift" Fae.Signal.lift ::
        Fae.Unary -> Fae.Ptr -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_lift2" Fae.Signal.lift2 ::
        Fae.Binary -> Fae.Ptr -> Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_lift3" Fae.Signal.lift3 ::
        Fae.Ternary ->
          Fae.Ptr -> Fae.Signal -> Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_apply" Fae.Signal.apply ::
        Fae.Processor -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_apply2" Fae.Signal.apply2
        :: Fae.Processor -> Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_apply3" Fae.Signal.apply3
        ::
        Fae.Processor ->
          Fae.Signal -> Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_constant"
        Fae.Signal.constant :: Fae.Ptr -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_value" Fae.Signal.value ::
        Fae.Event -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_delay" Fae.Signal.delay ::
        Fae.Time -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_add" Fae.Signal.add ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_subtract"
        Fae.Signal.subtract :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_multiply"
        Fae.Signal.multiply :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_power" Fae.Signal.power ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_divide" Fae.Signal.divide
        :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_modulo" Fae.Signal.modulo
        :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_absolute"
        Fae.Signal.absolute :: Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_not" Fae.Signal.not ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_and" Fae.Signal.and ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_or" Fae.Signal.or ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_xor" Fae.Signal.xor ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_bit_not" Fae.Signal.bitNot
        :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_bit_and" Fae.Signal.bitAnd
        :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_bit_or" Fae.Signal.bitOr ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_bit_xor" Fae.Signal.bitXor
        :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_shift_left"
        Fae.Signal.shiftLeft :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_shift_right"
        Fae.Signal.shiftRight :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_equal" Fae.Signal.equal ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_less_than"
        Fae.Signal.lessThan :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_greater_than"
        Fae.Signal.greaterThan :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_less_than_equal"
        Fae.Signal.lessThanEqual :: Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_greater_than_equal"
        Fae.Signal.greaterThanEqual ::
        Fae.Signal -> Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_acos" Fae.Signal.acos ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_asin" Fae.Signal.asin ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_atan" Fae.Signal.atan ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_cos" Fae.Signal.cos ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_sin" Fae.Signal.sin ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_tan" Fae.Signal.tan ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_exp" Fae.Signal.exp ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_log" Fae.Signal.log ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_log10" Fae.Signal.log10 ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_pow" Fae.Signal.pow ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_sqrt" Fae.Signal.sqrt ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_abs" Fae.Signal.abs ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_min" Fae.Signal.min ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_max" Fae.Signal.max ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_fmod" Fae.Signal.fmod ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_remainder"
        Fae.Signal.remainder :: Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_floor" Fae.Signal.floor ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_ceil" Fae.Signal.ceil ::
        Fae.Signal -> Fae.Signal
 
foreign import ccall unsafe "fae_signal_rint" Fae.Signal.rint ::
        Fae.Signal -> Fae.Signal

