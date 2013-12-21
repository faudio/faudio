module Fa.Audio where
import Foreign
import Foreign.C
import Fa
import Fa.Time
import Fa.String
import Fa.List
import Fa.Pair
import Fa.Error
import Fa.Signal
import Fa.Action
import Fa.Clock
 
data Session_
 
type Session = Ptr Session_
 
data Device_
 
type Device = Ptr Device_
 
data Stream_
 
type Stream = Ptr Stream_
 
type SessionCallback = FunPtr (FaPtr -> Session -> Session)
 
type StreamCallback = FunPtr (FaPtr -> Stream -> Stream)
 
type StatusCallback = Nullary
 
type MessageCallback = Unary
 
type Proc = FunPtr (FaPtr -> List -> List)
 
foreign import ccall unsafe "fa_audio_begin_session" beginSession
        :: IO Session
 
foreign import ccall unsafe "fa_audio_end_session" endSession ::
        Session -> IO ()
 
foreign import ccall unsafe "fa_audio_with_session" withSession ::
        SessionCallback -> FaPtr -> Callback -> FaPtr -> IO ()
 
foreign import ccall unsafe "fa_audio_set_parameter" setParameter
        :: FaString -> FaPtr -> Session -> IO ()
 
foreign import ccall unsafe "fa_audio_current_sessions"
        currentSessions :: IO List
 
foreign import ccall unsafe "fa_audio_end_all_sessions"
        endAllSessions :: IO FaPtr
 
foreign import ccall unsafe "fa_audio_all" all ::
        Session -> IO List
 
foreign import ccall unsafe "fa_audio_default" default' ::
        Session -> IO Pair
 
foreign import ccall unsafe "fa_audio_default_input" defaultInput
        :: Session -> IO Device
 
foreign import ccall unsafe "fa_audio_default_output" defaultOutput
        :: Session -> IO Device
 
foreign import ccall unsafe "fa_audio_add_status_callback"
        addStatusCallback :: StatusCallback -> FaPtr -> Session -> IO ()
 
foreign import ccall unsafe "fa_audio_session" session ::
        Device -> IO Session
 
foreign import ccall unsafe "fa_audio_name" name ::
        Device -> IO FaString
 
foreign import ccall unsafe "fa_audio_host_name" hostName ::
        Device -> IO FaString
 
foreign import ccall unsafe "fa_audio_has_input" hasInput ::
        Device -> IO CInt
 
foreign import ccall unsafe "fa_audio_has_output" hasOutput ::
        Device -> IO CInt
 
foreign import ccall unsafe "fa_audio_input_channels" inputChannels
        :: Device -> IO CInt
 
foreign import ccall unsafe "fa_audio_output_channels"
        outputChannels :: Device -> IO CInt

-- TODO must be safe 
foreign import ccall safe "fa_audio_open_stream" openStream ::
        Device -> Device -> Proc -> FaPtr -> IO Stream
 
foreign import ccall unsafe "fa_audio_close_stream" closeStream ::
        Stream -> IO ()
 
foreign import ccall unsafe "fa_audio_with_stream" withStream ::
        Device ->
          Device ->
            Proc -> FaPtr -> StreamCallback -> FaPtr -> Callback -> FaPtr -> IO ()
 
foreign import ccall unsafe "fa_audio_devices" devices ::
        Stream -> IO List
 
foreign import ccall unsafe "fa_audio_get_clock" getClock ::
        Stream -> IO Clock
 
foreign import ccall unsafe "fa_audio_stream_clock" streamClock ::
        Stream -> IO Clock
 
foreign import ccall unsafe "fa_audio_add_message_callback"
        addMessageCallback :: MessageCallback -> FaPtr -> Stream -> IO ()
 
foreign import ccall unsafe "fa_audio_schedule" schedule ::
        Time -> Action -> Stream -> IO ()
 
foreign import ccall unsafe "fa_audio_schedule_relative"
        scheduleRelative :: Time -> Action -> Stream -> IO ()

