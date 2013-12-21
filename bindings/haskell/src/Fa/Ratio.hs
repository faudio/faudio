module Fa.Ratio where
import Foreign
import Foreign.C
import Fa.Std
 
type Numerator = Int32
 
type Denominator = Int32
 
data Ratio_
 
type Ratio = Ptr Ratio_
 
foreign import ccall unsafe "fa_ratio_create" create ::
        Numerator -> Denominator -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_num" num ::
        Ratio -> IO Numerator
 
foreign import ccall unsafe "fa_ratio_denom" denom ::
        Ratio -> IO Denominator
 
foreign import ccall unsafe "fa_ratio_match" match ::
        Ratio -> Ptr Numerator -> Ptr Denominator -> IO ()
 
foreign import ccall unsafe "fa_ratio_copy" copy ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_destroy" destroy ::
        Ratio -> IO ()
 
foreign import ccall unsafe "fa_ratio_add" add ::
        Ratio -> Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_subtract" subtract ::
        Ratio -> Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_multiply" multiply ::
        Ratio -> Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_divide" divide ::
        Ratio -> Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_succ" succ ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_pred" pred ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_negate" negate ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_recip" recip ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_absolute" absolute ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_normalize" normalize ::
        Ratio -> IO Ratio
 
foreign import ccall unsafe "fa_ratio_to_mixed" toMixed ::
        Ratio -> Ptr Numerator -> Ptr Ratio -> IO ()

