module Fa.Atomic where
import Foreign
import Foreign.C
import Fa
import Fa.Std
 
data Atomic_
 
type Atomic = Ptr Atomic_
 
foreign import ccall unsafe "fa_atomic_create" create :: IO Atomic
 
foreign import ccall unsafe "fa_atomic_copy" copy ::
        Atomic -> IO Atomic
 
foreign import ccall unsafe "fa_atomic_destroy" destroy ::
        Atomic -> IO ()
 
foreign import ccall unsafe "fa_atomic_exchange" exchange ::
        Atomic -> Ptr -> Ptr -> IO CInt
 
foreign import ccall unsafe "fa_atomic_get" get :: Atomic -> IO Ptr
 
foreign import ccall unsafe "fa_atomic_modify" modify ::
        Atomic -> Unary -> Ptr -> IO ()
 
foreign import ccall unsafe "fa_atomic_set" set ::
        Atomic -> Ptr -> IO ()

