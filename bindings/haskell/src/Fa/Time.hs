module Fa.Time where
import Foreign
import Foreign.C
import Fa
import Fa.Ratio
import Fa.String
 
data Time_
 
type Time = Ptr Time_
 
type Days = Int32
 
type Hours = Int32
 
type Minutes = Int32
 
type Seconds = Int32
 
type Milliseconds = Int64
 
foreign import ccall unsafe "fa_time_create" create ::
        Days -> Hours -> Minutes -> Ratio -> IO Time
 
foreign import ccall unsafe "fa_time_copy" copy :: Time -> IO Time
 
foreign import ccall unsafe "fa_time_destroy" destroy ::
        Time -> IO ()
 
foreign import ccall unsafe "fa_time_days" days :: Time -> IO Int32
 
foreign import ccall unsafe "fa_time_hours" hours ::
        Time -> IO Int32
 
foreign import ccall unsafe "fa_time_minutes" minutes ::
        Time -> IO Int32
 
foreign import ccall unsafe "fa_time_seconds" seconds ::
        Time -> IO Int32
 
foreign import ccall unsafe "fa_time_divisions" divisions ::
        Time -> IO Ratio
 
foreign import ccall unsafe "fa_time_to_iso" toIso ::
        Time -> IO FaString
 
foreign import ccall unsafe "fa_time_to_seconds" toSeconds ::
        Time -> IO Seconds
 
foreign import ccall unsafe "fa_time_to_milliseconds"
        toMilliseconds :: Time -> IO Milliseconds
 
data System_
 
type System = Ptr System_

