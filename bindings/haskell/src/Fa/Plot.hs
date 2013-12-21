module Fa.Plot where
import Foreign
import Foreign.C
import Fa
import Fa.Std
import Fa.Buffer
 
type Function = Ptr (Ptr -> CInt -> CDouble -> CDouble -> CDouble)
 
foreign import ccall unsafe "fa_plot_use_gnu" useGnu :: IO ()
 
foreign import ccall unsafe "fa_plot_continous" continous ::
        Function -> Ptr -> Nullary -> Ptr -> IO ()
 
foreign import ccall unsafe "fa_plot_buffer_float" bufferFloat ::
        Buffer -> Nullary -> Ptr -> IO ()
 
foreign import ccall unsafe "fa_plot_buffer_double" bufferDouble ::
        Buffer -> Nullary -> Ptr -> IO ()

