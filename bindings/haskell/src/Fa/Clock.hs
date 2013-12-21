module Fa.Clock where
import Foreign
import Foreign.C
import Fa
import Fa.Time
 
type Interface = Ptr ()
 
data Clock_
 
type Clock = Ptr Clock_
 
foreign import ccall unsafe "fa_clock_time" time ::
        Clock -> IO Time
 
foreign import ccall unsafe "fa_clock_milliseconds" milliseconds ::
        Clock -> IO Milliseconds
 
foreign import ccall unsafe "fa_clock_standard" standard ::
        IO Clock

