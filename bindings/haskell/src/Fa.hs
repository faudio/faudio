module Fa where
import Foreign
import Foreign.C
import Fa.Std
 
type FaPtr = Ptr ()
 
type Nullary = Ptr (FaPtr -> FaPtr)
 
type Unary = Ptr (FaPtr -> FaPtr -> FaPtr)
 
type Binary = Ptr (FaPtr -> FaPtr -> FaPtr -> FaPtr)
 
type Ternary = Ptr (FaPtr -> FaPtr -> FaPtr -> FaPtr -> FaPtr)
 
type Pred = Ptr (FaPtr -> FaPtr -> CInt)
 
type Char8 = CChar
 
type Char16 = Word16
 
type Char32 = Word32
 
foreign import ccall unsafe "fa_is_bool" isBool :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_int8" isInt8 :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_int16" isInt16 :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_int32" isInt32 :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_int64" isInt64 :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_float" isFloat :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_double" isDouble ::
        FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_is_ref" isRef :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_to_bool" toBool :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_to_int8" toInt8 :: FaPtr -> IO Int8
 
foreign import ccall unsafe "fa_to_int16" toInt16 ::
        FaPtr -> IO Int16
 
foreign import ccall unsafe "fa_to_int32" toInt32 ::
        FaPtr -> IO Int32
 
foreign import ccall unsafe "fa_to_int64" toInt64 ::
        FaPtr -> IO Int64
 
foreign import ccall unsafe "fa_to_float" toFloat ::
        FaPtr -> IO CFloat
 
foreign import ccall unsafe "fa_to_double" toDouble ::
        FaPtr -> IO CDouble
 
-- foreign import ccall unsafe "fa_peek_bool" peekBool ::
--         FaPtr -> IO CInt
--  
-- foreign import ccall unsafe "fa_peek_int8" peekInt8 ::
--         FaPtr -> IO Int8
--  
-- foreign import ccall unsafe "fa_peek_int16" peekInt16 ::
--         FaPtr -> IO Int16
 
foreign import ccall unsafe "fa_peek_int32" peekInt32 ::
        FaPtr -> IO Int32
 
foreign import ccall unsafe "fa_peek_int64" peekInt64 ::
        FaPtr -> IO Int64
 
foreign import ccall unsafe "fa_peek_float" peekFloat ::
        FaPtr -> IO CFloat
 
foreign import ccall unsafe "fa_peek_double" peekDouble ::
        FaPtr -> IO CDouble
 
foreign import ccall unsafe "fa_from_bool" fromBool ::
        CInt -> IO FaPtr
 
foreign import ccall unsafe "fa_from_int8" fromInt8 ::
        Int8 -> IO FaPtr
 
foreign import ccall unsafe "fa_from_int16" fromInt16 ::
        Int16 -> IO FaPtr
 
foreign import ccall unsafe "fa_from_int32" fromInt32 ::
        Int32 -> IO FaPtr
 
foreign import ccall unsafe "fa_from_int64" fromInt64 ::
        Int64 -> IO FaPtr
 
foreign import ccall unsafe "fa_from_float" fromFloat ::
        CFloat -> IO FaPtr
 
foreign import ccall unsafe "fa_from_double" fromDouble ::
        CDouble -> IO FaPtr
 
type Id = Int64
 
type Impl = Ptr (Id -> FaPtr)
 
foreign import ccall unsafe "fa_interface" interface ::
        Id -> FaPtr -> IO FaPtr
 
type Equal = Ptr ()
 
foreign import ccall unsafe "fa_equal" equal ::
        FaPtr -> FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_not_equal" notEqual ::
        FaPtr -> FaPtr -> IO CInt
 
type Order = Ptr ()
 
foreign import ccall unsafe "fa_less_than" lessThan ::
        FaPtr -> FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_greater_than" greaterThan ::
        FaPtr -> FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_less_than_equal" lessThanEqual ::
        FaPtr -> FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_greater_than_equal"
        greaterThanEqual :: FaPtr -> FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_min" min :: FaPtr -> FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_max" max :: FaPtr -> FaPtr -> IO FaPtr
 
type Number = Ptr ()
 
foreign import ccall unsafe "fa_add" add :: FaPtr -> FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_subtract" subtract ::
        FaPtr -> FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_multiply" multiply ::
        FaPtr -> FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_divide" divide ::
        FaPtr -> FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_absolute" absolute :: FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_dadd" dadd :: FaPtr -> FaPtr -> IO FaPtr
 
-- foreign import ccall unsafe "fa_dsubtract" dsubtract ::
--         FaPtr -> FaPtr -> IO FaPtr
--  
-- foreign import ccall unsafe "fa_dmultiply" dmultiply ::
--         FaPtr -> FaPtr -> IO FaPtr
--  
-- foreign import ccall unsafe "fa_ddivide" ddivide ::
--         FaPtr -> FaPtr -> IO FaPtr
--  
-- foreign import ccall unsafe "fa_dabsolute" dabsolute ::
--         FaPtr -> IO FaPtr
 
type Copy = Ptr ()
 
type Destroy = Ptr ()
 
foreign import ccall unsafe "fa_copy" copy :: FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_move" move :: FaPtr -> IO FaPtr
 
foreign import ccall unsafe "fa_destroy" destroy :: FaPtr -> IO ()
 
foreign import ccall unsafe "fa_check" check :: FaPtr -> IO CInt
 
foreign import ccall unsafe "fa_print" print ::
        Ptr CChar -> FaPtr -> IO ()
 
foreign import ccall unsafe "fa_dprint" dprint ::
        Ptr CChar -> FaPtr -> IO ()
 
foreign import ccall unsafe "fa_print_ln" printLn :: FaPtr -> IO ()
 
foreign import ccall unsafe "fa_dprint_ln" dprintLn :: FaPtr -> IO ()

