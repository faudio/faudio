module Fa.Buffer where
import Foreign
import Foreign.C
import Fa
import Fa.Std
import Fa.Pair
import Fa.String
 
data Buffer_
 
type Buffer = Ptr Buffer_
 
foreign import ccall unsafe "fa_buffer_create" create ::
        CSize -> IO Buffer
 
foreign import ccall unsafe "fa_buffer_wrap" wrap ::
        FaPtr -> CSize -> Unary -> FaPtr -> IO Buffer
 
foreign import ccall unsafe "fa_buffer_copy" copy ::
        Buffer -> IO Buffer
 
foreign import ccall unsafe "fa_buffer_resize" resize ::
        CSize -> Buffer -> IO Buffer
 
foreign import ccall unsafe "fa_buffer_dresize" dresize ::
        CSize -> Buffer -> IO Buffer
 
foreign import ccall unsafe "fa_buffer_destroy" destroy ::
        Buffer -> IO ()
 
foreign import ccall unsafe "fa_buffer_size" size ::
        Buffer -> IO CSize
 
foreign import ccall unsafe "fa_buffer_get" get ::
        Buffer -> CSize -> IO Word8
 
foreign import ccall unsafe "fa_buffer_set" set ::
        Buffer -> CSize -> Word8 -> IO ()
 
foreign import ccall unsafe "fa_buffer_get_int16" getInt16 ::
        Buffer -> CSize -> IO Int16
 
foreign import ccall unsafe "fa_buffer_get_int32" getInt32 ::
        Buffer -> CSize -> IO Int32
 
foreign import ccall unsafe "fa_buffer_get_int64" getInt64 ::
        Buffer -> CSize -> IO Int64
 
foreign import ccall unsafe "fa_buffer_get_float" getFloat ::
        Buffer -> CSize -> IO CFloat
 
foreign import ccall unsafe "fa_buffer_get_double" getDouble ::
        Buffer -> CSize -> IO CDouble
 
foreign import ccall unsafe "fa_buffer_set_int16" setInt16 ::
        Buffer -> CSize -> Int16 -> IO ()
 
foreign import ccall unsafe "fa_buffer_set_int32" setInt32 ::
        Buffer -> CSize -> Int32 -> IO ()
 
foreign import ccall unsafe "fa_buffer_set_int64" setInt64 ::
        Buffer -> CSize -> Int64 -> IO ()
 
foreign import ccall unsafe "fa_buffer_set_float" setFloat ::
        Buffer -> CSize -> CFloat -> IO ()
 
foreign import ccall unsafe "fa_buffer_set_double" setDouble ::
        Buffer -> CSize -> CDouble -> IO ()
 
foreign import ccall unsafe "fa_buffer_read_raw" readRaw ::
        FaString -> IO Buffer
 
foreign import ccall unsafe "fa_buffer_write_raw" writeRaw ::
        FaString -> Buffer -> IO ()
 
foreign import ccall unsafe "fa_buffer_read_audio" readAudio ::
        FaString -> IO Pair
 
foreign import ccall unsafe "fa_buffer_write_audio" writeAudio ::
        FaString -> CInt -> Buffer -> IO FaPtr
 
foreign import ccall unsafe "fa_buffer_unsafe_address"
        unsafeAddress :: Buffer -> IO (Ptr ())

