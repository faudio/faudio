module Fa.Action where
import Foreign
import Foreign.C
import Fa
import Fa.Pair
import Fa.List
import Fa.Signal
import Fa.Time
 
data Action_
 
type Action = Ptr Action_
 
type Channel = CInt
 
-- type Name = FaString
 
type Value = FaPtr
 
foreign import ccall unsafe "fa_action_null" null :: IO Action
 
foreign import ccall unsafe "fa_action_copy" copy ::
        Action -> IO Action
 
foreign import ccall unsafe "fa_action_destroy" destroy ::
        Action -> IO ()
 
foreign import ccall unsafe "fa_action_set" set ::
        Channel -> CDouble -> IO Action
 
foreign import ccall unsafe "fa_action_accum" accum ::
        Channel -> UnaryDouble -> FaPtr -> IO Action
 
foreign import ccall unsafe "fa_action_send" send ::
        Name -> Value -> IO Action
 
foreign import ccall unsafe "fa_action_is_set" isSet ::
        Action -> IO CInt
 
foreign import ccall unsafe "fa_action_set_channel" setChannel ::
        Action -> IO Channel
 
foreign import ccall unsafe "fa_action_set_value" setValue ::
        Action -> IO CDouble
 
foreign import ccall unsafe "fa_action_is_accum" isAccum ::
        Action -> IO CInt
 
foreign import ccall unsafe "fa_action_accum_channel" accumChannel
        :: Action -> IO Channel
 
foreign import ccall unsafe "fa_action_accum_function"
        accumFunction :: Action -> IO UnaryDouble
 
foreign import ccall unsafe "fa_action_accum_data" accumData ::
        Action -> IO FaPtr
 
foreign import ccall unsafe "fa_action_is_send" isSend ::
        Action -> IO CInt
 
foreign import ccall unsafe "fa_action_send_name" sendName ::
        Action -> IO Name
 
foreign import ccall unsafe "fa_action_send_value" sendValue ::
        Action -> IO Value
 
foreign import ccall unsafe "fa_action_repeat" repeat ::
        Time -> Action -> IO Action
 
foreign import ccall unsafe "fa_action_many" many ::
        List -> IO Action
 
foreign import ccall unsafe "fa_action_if" if' ::
        Pred -> FaPtr -> Action -> IO Action
 
foreign import ccall unsafe "fa_action_while" while ::
        Pred -> FaPtr -> Action -> IO Action
 
foreign import ccall unsafe "fa_action_until" until ::
        Pred -> FaPtr -> Action -> IO Action
 
foreign import ccall unsafe "fa_action_do" do' ::
        Nullary -> FaPtr -> IO Action
 
foreign import ccall unsafe "fa_action_is_simple" isSimple ::
        Action -> IO CInt
 
foreign import ccall unsafe "fa_action_is_compound" isCompound ::
        Action -> IO CInt
 
foreign import ccall unsafe "fa_action_compound_interval"
        compoundInterval :: Action -> IO Time
 
foreign import ccall unsafe "fa_action_compound_first"
        compoundFirst :: Action -> IO Action
 
foreign import ccall unsafe "fa_action_compound_rest" compoundRest
        :: Action -> IO Action

