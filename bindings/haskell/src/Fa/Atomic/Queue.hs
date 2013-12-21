module Fa.Atomic.Queue where
import Foreign
import Foreign.C
import Fa
 
data Queue_
 
type Queue = Ptr Queue_
 
foreign import ccall unsafe "fa_atomic_queue_create" create ::
        IO Queue
 
foreign import ccall unsafe "fa_atomic_queue_destroy" destroy ::
        Queue -> IO ()
 
foreign import ccall unsafe "fa_atomic_queue_read" read ::
        Queue -> IO Ptr
 
foreign import ccall unsafe "fa_atomic_queue_write" write ::
        Queue -> Ptr -> IO CInt

