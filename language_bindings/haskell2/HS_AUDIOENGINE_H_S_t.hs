
{-# OPTIONS -fglasgow-exts -XForeignFunctionInterface #-}
module HS_AUDIOENGINE_H_S_t (
  module HS_AUDIOENGINE_H_S_t
) where

import Foreign
import Foreign.C.Types
import HS_AUDIOENGINE_H_S_n

type T_SclAction = FunPtr (CInt -> IO (()))
type T_SclAtom = Ptr (CChar)
type T_SclAtomType = CInt
type T_SclAudioDevice = Ptr (CChar)
type T_SclAudioHost = Ptr (CChar)
type T_SclAudioProcessor = Ptr (CChar)
type T_SclBufferedReceiver = FunPtr (CInt -> Ptr (Ptr (Ptr (CChar))) -> Ptr (CInt) -> CInt -> IO (()))
type T_SclChar = CChar
type T_SclDeviceStreamOptions = Ptr (CChar)
type T_SclDspError = Ptr (CChar)
type T_SclError = Ptr (CChar)
type T_SclErrorHandler = FunPtr (CInt -> Ptr (CChar) -> IO (()))
type T_SclFloat32 = CFloat
type T_SclFloat64 = CDouble
type T_SclFuture = Ptr (CChar)
type T_SclFutureGroup = Ptr (CChar)
type T_SclInt16 = CShort
type T_SclInt32 = CInt
type T_SclInt64 = CLLong
type T_SclInterruptable = Ptr (CChar)
type T_SclInterruptionMode = CInt
type T_SclMessageInfo = Ptr (CChar)
type T_SclMessageKind = CInt
type T_SclMidiDevice = Ptr (CChar)
type T_SclPortaudioError = Ptr (CChar)
type T_SclPortmidiError = Ptr (CChar)
type T_SclRealTime = CDouble
type T_SclReceiveOptions = Ptr (CChar)
type T_SclReceiver = FunPtr (CInt -> Ptr (Ptr (CChar)) -> CInt -> IO (()))
type T_SclScheduleOptions = Ptr (CChar)
type T_SclSendOptions = Ptr (CChar)
type T_SclStream = Ptr (CChar)
type T_SclStreamError = Ptr (CChar)
type T_SclStreamType = CInt
type T_SclString = Ptr (CChar)
type T_SclTime = CInt
type T_SclTimeUnit = CInt
type T_SclWord16 = CUShort
type T_SclWord32 = CUInt
type T_SclWord64 = CULLong
type T_int16_t = CShort
type T_int32_t = CInt
type T_int64_t = CLLong
type T_int8_t = CSChar
type T_int_fast16_t = CShort
type T_int_fast32_t = CInt
type T_int_fast64_t = CLLong
type T_int_fast8_t = CSChar
type T_int_least16_t = CShort
type T_int_least32_t = CInt
type T_int_least64_t = CLLong
type T_int_least8_t = CSChar
type T_intmax_t = CLong
type T_intptr_t = CLong
type T_uint16_t = CUShort
type T_uint32_t = CUInt
type T_uint64_t = CULLong
type T_uint8_t = CUChar
type T_uint_fast16_t = CUShort
type T_uint_fast32_t = CUInt
type T_uint_fast64_t = CULLong
type T_uint_fast8_t = CUChar
type T_uint_least16_t = CUShort
type T_uint_least32_t = CUInt
type T_uint_least64_t = CULLong
type T_uint_least8_t = CUChar
type T_uintmax_t = Unmapped_C_Type_long_unsigned_int
type T_uintptr_t = CULong

