module Fa.Fa where
import Foreign
import Foreign.C
import Fa
import Fa.List
import Fa.String
import Fa.Error
-- import Fa.Atomic
-- import Fa.Atomic.Queue
-- import Fa.Atomic.Stack
-- import Fa.Atomic.RingBuffer
-- import Fa.System
-- import Fa.Error
-- import Fa.Pair
-- import Fa.Pair.Left
-- import Fa.List
-- import Fa.Graph
-- import Fa.Audio
-- import Fa.Midi
-- import Fa.Midi.Message
-- import Fa.Plot
-- import Fa.Thread
-- import Fa.Time
-- import Fa.PriorityQueue
-- import Fa.Action
-- import Fa.Signal   
 
foreign import ccall unsafe "fa_fa_version" version :: IO List
 
foreign import ccall unsafe "fa_fa_version_string" versionString ::
        IO FaString
 
foreign import ccall unsafe "fa_fa_initialize" initialize :: IO ()
 
foreign import ccall unsafe "fa_fa_terminate" terminate :: IO ()
 
-- type LogFunc = Ptr (FaPtr -> System -> Error -> ())
 
foreign import ccall unsafe "fa_fa_set_log_file" setLogFile ::
        FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_set_log_std" setLogStd :: IO ()
 
-- foreign import ccall unsafe "fa_fa_set_log" setLog ::
        -- LogFunc -> FaPtr -> IO ()
 
foreign import ccall unsafe "fa_fa_log" log ::
        FaPtr -> Error -> IO ()
 
foreign import ccall unsafe "fa_fa_log_info" logInfo ::
        FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_log_warning" logWarning ::
        FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_log_error" logError ::
        FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_log_info_from" logInfoFrom ::
        FaString -> FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_log_warning_from" logWarningFrom
        :: FaString -> FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_log_error_from" logErrorFrom ::
        FaString -> FaString -> IO ()
 
foreign import ccall unsafe "fa_fa_dlog" dlog ::
        FaPtr -> Error -> IO ()
 
foreign import ccall unsafe "fa_fa_dlog_info" dlogInfo ::
        FaString -> IO ()

