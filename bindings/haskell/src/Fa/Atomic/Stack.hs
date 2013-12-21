module Fa.Atomic.Stack where
import Foreign
import Foreign.C
import Fa
 
data Stack_
 
type Stack = Ptr Stack_
 
foreign import ccall unsafe "fa_atomic_stack_create" create ::
        IO Stack
 
foreign import ccall unsafe "fa_atomic_stack_destroy" destroy ::
        Stack -> IO ()
 
foreign import ccall unsafe "fa_atomic_stack_read" read ::
        Stack -> IO Ptr
 
foreign import ccall unsafe "fa_atomic_stack_write" write ::
        Stack -> Ptr -> IO CInt

