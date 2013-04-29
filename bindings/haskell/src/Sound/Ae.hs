
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, BangPatterns #-}

module Sound.Ae where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C 
import Data.Word
import Data.Bits 

data AeList
data AeString

foreign import ccall "doremir_audio_engine_version"            
    c_Version :: IO (Ptr AeList)
foreign import ccall "doremir_audio_engine_version_string"            
    c_VersionString :: IO (Ptr AeString)
foreign import ccall "doremir_string_to_utf8"            
    c_StringToUtf8 :: Ptr AeString -> IO CString


