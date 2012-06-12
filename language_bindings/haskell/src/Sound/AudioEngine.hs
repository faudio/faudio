{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.AudioEngine where

import Foreign
import Foreign.C.Types
 
foreign import ccall unsafe "math.h sin"
     c_sin :: CDouble -> CDouble
     
foreign import ccall unsafe "stdlib.h rand"
     c_rand :: IO CUInt
 
foreign import ccall "stdlib.h srand"
     c_srand :: CUInt -> IO ()
     

foreign import ccall "sclaudio.h scl_message_type_audio"
    scl_message_type_audio :: IO CInt
    
          
main = putStrLn "Hello"