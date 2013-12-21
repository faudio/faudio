module Fa.Atomic.RingBuffer where
import Foreign
import Foreign.C
import Fa
 
data RingBuffer_
 
type RingBuffer = Ptr RingBuffer_
 
foreign import ccall unsafe "fa_atomic_ring_buffer_create" create
        :: CSize -> IO RingBuffer
 
foreign import ccall unsafe "fa_atomic_ring_buffer_destroy" destroy
        :: RingBuffer -> IO ()
 
foreign import ccall unsafe "fa_atomic_ring_buffer_size" size ::
        RingBuffer -> IO CSize
 
foreign import ccall unsafe "fa_atomic_ring_buffer_read" read ::
        RingBuffer -> IO Word8
 
foreign import ccall unsafe "fa_atomic_ring_buffer_read_float"
        readFloat :: RingBuffer -> IO CFloat
 
foreign import ccall unsafe "fa_atomic_ring_buffer_read_double"
        readDouble :: RingBuffer -> IO CDouble
 
foreign import ccall unsafe "fa_atomic_ring_buffer_write" write ::
        RingBuffer -> Word8 -> IO CInt
 
foreign import ccall unsafe "fa_atomic_ring_buffer_write_float"
        writeFloat :: RingBuffer -> CFloat -> IO CInt
 
foreign import ccall unsafe "fa_atomic_ring_buffer_write_double"
        writeDouble :: RingBuffer -> CDouble -> IO CInt

