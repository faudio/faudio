module Fa.Midi.Message where
import Foreign
import Foreign.C
import Fa.Std
import Fa.Pair
import Fa.Buffer
 
type Status = CInt
 
type Channel = CInt
 
type Data = CInt
 
data Message_
 
type Message = Ptr Message_
 
foreign import ccall unsafe "fa_midi_message_create_simple"
        createSimple :: Status -> CInt -> CInt -> IO Message
 
foreign import ccall unsafe "fa_midi_message_create_sysex"
        createSysex :: Buffer -> IO Message
 
foreign import ccall unsafe "fa_midi_message_copy" copy ::
        Message -> IO Message
 
foreign import ccall unsafe "fa_midi_message_destroy" destroy ::
        Message -> IO ()
 
foreign import ccall unsafe "fa_midi_message_is_simple" isSimple ::
        Message -> IO CInt
 
foreign import ccall unsafe "fa_midi_message_simple_data"
        simpleData :: Message -> IO Pair
 
foreign import ccall unsafe "fa_midi_message_status" status ::
        Message -> IO Status
 
foreign import ccall unsafe "fa_midi_message_channel" channel ::
        Message -> IO Channel
 
foreign import ccall unsafe "fa_midi_message_is_sysex" isSysex ::
        Message -> IO CInt
 
foreign import ccall unsafe "fa_midi_message_sysex_data" sysexData
        :: Message -> IO Buffer

