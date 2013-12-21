module Fa.String where
import Foreign
import Foreign.C
import Fa
 
type Utf8 = Ptr Char8
 
type Cp1252 = Ptr Char8
 
type Utf16 = Ptr Char16
 
type Utf32 = Ptr Char32
 
data FaString_
 
type FaString = Ptr FaString_
 
foreign import ccall unsafe "fa_string_empty" empty :: IO FaString
 
foreign import ccall unsafe "fa_string_single" single ::
        Char16 -> IO FaString
 
foreign import ccall unsafe "fa_string_repeat" repeat ::
        CInt -> Char16 -> IO FaString
 
foreign import ccall unsafe "fa_string_copy" copy ::
        FaString -> IO FaString
 
foreign import ccall unsafe "fa_string_append" append ::
        FaString -> FaString -> IO FaString
 
foreign import ccall unsafe "fa_string_dappend" dappend ::
        FaString -> FaString -> IO FaString
 
foreign import ccall unsafe "fa_string_destroy" destroy ::
        FaString -> IO ()
 
foreign import ccall unsafe "fa_string_length" length ::
        FaString -> IO CInt
 
foreign import ccall unsafe "fa_string_char_at" charAt ::
        CInt -> FaString -> IO Char16
 
type Show = Ptr ()
 
foreign import ccall unsafe "fa_string_show" show ::
        FaPtr -> IO FaString
 
foreign import ccall unsafe "fa_string_to_string" toFaString ::
        FaPtr -> IO FaString
 
foreign import ccall unsafe "fa_string_to_json" toJson ::
        FaPtr -> IO FaString
 
foreign import ccall unsafe "fa_string_from_json" fromJson ::
        FaString -> IO FaPtr
 
foreign import ccall unsafe "fa_string_to_utf8" toUtf8 ::
        FaString -> IO Utf8
 
foreign import ccall unsafe "fa_string_to_cp1252" toCp1252 ::
        FaString -> IO Cp1252
 
foreign import ccall unsafe "fa_string_to_utf16" toUtf16 ::
        FaString -> IO Utf16
 
foreign import ccall unsafe "fa_string_to_native" toNative ::
        FaString -> IO FaPtr
 
foreign import ccall unsafe "fa_string_from_utf8" fromUtf8 ::
        Utf8 -> IO FaString
 
foreign import ccall unsafe "fa_string_from_cp1252" fromCp1252 ::
        Cp1252 -> IO FaString
 
foreign import ccall unsafe "fa_string_from_utf16" fromUtf16 ::
        Utf16 -> IO FaString
 
foreign import ccall unsafe "fa_string_from_native" fromNative ::
        FaPtr -> IO FaString
 
foreign import ccall unsafe "fa_string_matches" matches ::
        FaString -> FaString -> IO CInt
 
foreign import ccall unsafe "fa_string_format_integral"
        formatIntegral :: Ptr CChar -> CLong -> IO FaString
 
foreign import ccall unsafe "fa_string_format_floating"
        formatFloating :: Ptr CChar -> CDouble -> IO FaString

