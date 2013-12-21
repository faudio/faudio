module Fa.Thread where
import Foreign
import Foreign.C
import Fa
import Fa.Std
import Fa.Time
 
data Thread_
 
type Thread = Ptr Thread_
 
data Mutex_
 
type Mutex = Ptr Mutex_
 
foreign import ccall unsafe "fa_thread_create" create ::
        Nullary -> Ptr -> IO Thread
 
foreign import ccall unsafe "fa_thread_join" join ::
        Thread -> IO ()
 
foreign import ccall unsafe "fa_thread_detach" detach ::
        Thread -> IO ()
 
foreign import ccall unsafe "fa_thread_to_native" toNative ::
        Thread -> IO Ptr
 
foreign import ccall unsafe "fa_thread_main" main :: IO Thread
 
foreign import ccall unsafe "fa_thread_current" current ::
        IO Thread
 
foreign import ccall unsafe "fa_thread_sleep" sleep ::
        Milliseconds -> IO ()
 
foreign import ccall unsafe "fa_thread_create_mutex" createMutex ::
        IO Mutex
 
foreign import ccall unsafe "fa_thread_destroy_mutex" destroyMutex
        :: Mutex -> IO ()
 
foreign import ccall unsafe "fa_thread_lock" lock ::
        Mutex -> IO CInt
 
foreign import ccall unsafe "fa_thread_try_lock" tryLock ::
        Mutex -> IO CInt
 
foreign import ccall unsafe "fa_thread_unlock" unlock ::
        Mutex -> IO CInt

