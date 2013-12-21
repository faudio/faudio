module Fa.Midi where
import Foreign
import Foreign.C
import Fa.List
import Fa.Pair
import Fa.Action
import Fa.Time
import Fa.Clock
import Fa.Error
 
data Device_
 
type Device = Ptr Device_
 
data Session_
 
type Session = Ptr Session_
 
data Stream_
 
type Stream = Ptr Stream_
 
type SessionCallback = Ptr (Ptr -> Session -> Session)
 
type StreamCallback = Ptr (Ptr -> Stream -> Stream)
 
type StatusCallback = Nullary
 
type MessageCallback = Unary
 
foreign import ccall unsafe "fa_midi_begin_session" beginSession ::
        IO Session
 
foreign import ccall unsafe "fa_midi_end_session" endSession ::
        Session -> IO ()
 
foreign import ccall unsafe "fa_midi_with_session" withSession ::
        SessionCallback -> Ptr -> Callback -> Ptr -> IO ()
 
foreign import ccall unsafe "fa_midi_current_sessions"
        currentSessions :: IO List
 
foreign import ccall unsafe "fa_midi_end_all_sessions"
        endAllSessions :: IO Ptr
 
foreign import ccall unsafe "fa_midi_all" all :: Session -> IO List
 
foreign import ccall unsafe "fa_midi_default" default ::
        Session -> IO Pair
 
foreign import ccall unsafe "fa_midi_default_input" defaultInput ::
        Session -> IO Device
 
foreign import ccall unsafe "fa_midi_default_output" defaultOutput
        :: Session -> IO Device
 
foreign import ccall unsafe "fa_midi_add_status_callback"
        addStatusCallback :: StatusCallback -> Ptr -> Session -> IO ()
 
foreign import ccall unsafe "fa_midi_name" name ::
        Device -> IO String
 
foreign import ccall unsafe "fa_midi_host_name" hostName ::
        Device -> IO String
 
foreign import ccall unsafe "fa_midi_has_input" hasInput ::
        Device -> IO CInt
 
foreign import ccall unsafe "fa_midi_has_output" hasOutput ::
        Device -> IO CInt
 
foreign import ccall unsafe "fa_midi_open_stream" openStream ::
        Device -> IO Stream
 
foreign import ccall unsafe "fa_midi_close_stream" closeStream ::
        Stream -> IO ()
 
foreign import ccall unsafe "fa_midi_with_stream" withStream ::
        Device -> StreamCallback -> Ptr -> Callback -> Ptr -> IO ()
 
foreign import ccall unsafe "fa_midi_add_message_callback"
        addMessageCallback :: MessageCallback -> Ptr -> Stream -> IO ()
 
foreign import ccall unsafe "fa_midi_set_clock" setClock ::
        Stream -> Clock -> IO ()
 
foreign import ccall unsafe "fa_midi_get_clock" getClock ::
        Stream -> IO Clock
 
foreign import ccall unsafe "fa_midi_schedule" schedule ::
        Time -> Action -> Stream -> IO ()
 
foreign import ccall unsafe "fa_midi_schedule_relative"
        scheduleRelative :: Time -> Action -> Stream -> IO ()

