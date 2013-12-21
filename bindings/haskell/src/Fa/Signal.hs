module Fa.Signal where
import Foreign
import Foreign.C
import Fa
import Fa.Pair
import Fa.Time
import Fa.Buffer
import Fa.List
import Fa.String
 
data Signal_
 
type Signal = Ptr Signal_
 
type UnarySignal = Ptr (FaPtr -> Signal -> Signal)
 
type UnaryDouble = Ptr (FaPtr -> CDouble -> CDouble)
 
type BinaryDouble = Ptr (FaPtr -> CDouble -> CDouble -> CDouble)
 
foreign import ccall unsafe "fa_signal_time" time :: IO Signal
 
foreign import ccall unsafe "fa_signal_random" random :: IO Signal
 
foreign import ccall unsafe "fa_signal_constant" constant ::
        CDouble -> IO Signal
 
foreign import ccall unsafe "fa_signal_lift" lift ::
        FaString -> UnaryDouble -> FaPtr -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_lift2" lift2 ::
        FaString -> BinaryDouble -> FaPtr -> Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_loop" loop ::
        UnarySignal -> FaPtr -> IO Signal
 
foreign import ccall unsafe "fa_signal_delay" delay ::
        CInt -> Signal -> IO Signal
 
type Name = FaString
 
type Message = FaPtr
 
type State = Ptr ()
 
type CustomProcessor = Ptr ()
 
-- foreign import ccall unsafe "fa_signal_custom" custom ::
        -- Ptr CustomProcessor -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_input" input ::
        CInt -> IO Signal
 
foreign import ccall unsafe "fa_signal_output" output ::
        CInt -> CInt -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_former" former ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_latter" latter ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_print" print ::
        CInt -> List -> Signal -> IO ()
 
foreign import ccall unsafe "fa_signal_run" run ::
        CInt -> List -> Signal -> Ptr CDouble -> IO ()
 
foreign import ccall unsafe "fa_signal_run_buffer" runBuffer ::
        CInt -> List -> Signal -> IO Buffer
 
foreign import ccall unsafe "fa_signal_run_file" runFile ::
        CInt -> List -> Signal -> FaString -> IO FaPtr
 
foreign import ccall unsafe "fa_signal_play" play ::
        Buffer -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_record" record ::
        Buffer -> Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_add" add ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_subtract" subtract ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_multiply" multiply ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_power" power ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_divide" divide ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_modulo" modulo ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_absolute" absolute ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_not" not :: IO Signal
 
foreign import ccall unsafe "fa_signal_and" and ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_or" or ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_xor" xor ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_equal" equal ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_less_than" lessThan ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_greater_than" greaterThan ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_less_than_equal"
        lessThanEqual :: Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_greater_than_equal"
        greaterThanEqual :: Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_acos" acos ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_asin" asin ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_atan" atan ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_cos" cos ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_sin" sin ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_tan" tan ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_exp" exp ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_log" log ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_log10" log10 ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_sqrt" sqrt ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_min" min ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_max" max ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_fmod" fmod ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_remainder" remainder ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_floor" floor ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_ceil" ceil ::
        Signal -> Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_counter" counter ::
        IO Signal
 
foreign import ccall unsafe "fa_signal_impulses" impulses ::
        CInt -> IO Signal
 
foreign import ccall unsafe "fa_signal_vst" vst ::
        FaString -> FaString -> List -> IO List
 
foreign import ccall unsafe "fa_signal_dls" dls :: IO Pair
 
foreign import ccall unsafe "fa_signal_synth" synth ::
        FaString -> IO Pair
 
foreign import ccall unsafe "fa_signal_to_tree" toTree ::
        Signal -> IO Pair
 
foreign import ccall unsafe "fa_signal_draw_tree" drawTree ::
        Pair -> IO FaString
 
foreign import ccall unsafe "fa_signal_simplify" simplify ::
        Signal -> IO Signal
 
foreign import ccall unsafe "fa_signal_impulse" impulse ::
        IO Signal
 
foreign import ccall unsafe "fa_signal_line" line ::
        CDouble -> IO Signal

