module Fa.Error where
import Foreign
import Foreign.C
import Fa
import Fa.String
 
type Severity = CInt
 
type Interface = Ptr ()
 
data Error_
 
type Error = Ptr Error_
 
foreign import ccall unsafe "fa_error_check" check ::
        FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_error_log" log ::
        FaPtr -> Error -> IO ()
 
foreign import ccall unsafe "fa_error_severity" severity ::
        Error -> IO Severity
 
foreign import ccall unsafe "fa_error_message" message ::
        Error -> IO FaString
 
foreign import ccall unsafe "fa_error_origin" origin ::
        Error -> IO FaString
 
foreign import ccall unsafe "fa_error_format" format ::
        CInt -> Error -> IO FaString
 
type Callback = Ptr (FaPtr -> Error -> ())
 
foreign import ccall unsafe "fa_error_create_simple" createSimple
        :: Severity -> FaString -> FaString -> IO Error

