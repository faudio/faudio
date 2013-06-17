
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, BangPatterns #-}

module Sound.Fae where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C 
import Data.Word
import Data.Bits 

data FaeList
data FaeString

foreign import ccall "fae_fae_version"            
    c_Version :: IO (Ptr FaeList)
foreign import ccall "fae_fae_version_string"            
    c_VersionString :: IO (Ptr FaeString)
foreign import ccall "fae_string_to_utf8"            
    c_StringToUtf8 :: Ptr FaeString -> IO CString


