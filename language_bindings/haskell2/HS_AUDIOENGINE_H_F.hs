
{-# OPTIONS -fglasgow-exts -XForeignFunctionInterface #-}
module HS_AUDIOENGINE_H_F (
  module HS_AUDIOENGINE_H_F
) where

import Foreign
import Foreign.C.Types
import HS_AUDIOENGINE_H_S
import HS_AUDIOENGINE_H_C
import HS_AUDIOENGINE_H_E
import HS_AUDIOENGINE_H_S_d
import HS_AUDIOENGINE_H_S_t
import HS_AUDIOENGINE_H_S_n

foreign import ccall "static hs_audioengine.h scl_message_type_audio"
  f_scl_message_type_audio :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_message_type_midi"
  f_scl_message_type_midi :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_time_unit_samples"
  f_scl_time_unit_samples :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_time_unit_milliseconds"
  f_scl_time_unit_milliseconds :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_atom_string"
  f_scl_atom_string :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_atom_int"
  f_scl_atom_int :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_atom_double"
  f_scl_atom_double :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_interruption_mode_simple"
  f_scl_interruption_mode_simple :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_interruption_mode_forcing"
  f_scl_interruption_mode_forcing :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_interruption_mode_transactional"
  f_scl_interruption_mode_transactional :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_schars_to_string"
  f_scl_schars_to_string :: Ptr (CSChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_uchars_to_string"
  f_scl_uchars_to_string :: Ptr (CUChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_string_to_schars"
  f_scl_string_to_schars :: Ptr (CChar) -> IO (Ptr (CSChar))

--

foreign import ccall "static hs_audioengine.h scl_string_to_uchars"
  f_scl_string_to_uchars :: Ptr (CChar) -> IO (Ptr (CUChar))

--

foreign import ccall "static hs_audioengine.h scl_atom_type"
  f_scl_atom_type :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_atom_to_int"
  f_scl_atom_to_int :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_atom_to_double"
  f_scl_atom_to_double :: Ptr (CChar) -> IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_atom_to_string"
  f_scl_atom_to_string :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_atom_from_int"
  f_scl_atom_from_int :: CInt -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_atom_from_double"
  f_scl_atom_from_double :: CDouble -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_atom_from_string"
  f_scl_atom_from_string :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_error_message"
  f_scl_error_message :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_portaudio_error_code"
  f_scl_portaudio_error_code :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_portmidi_error_code"
  f_scl_portmidi_error_code :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_host_name"
  f_scl_audio_host_name :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_audio_host_number_of_devices"
  f_scl_audio_host_number_of_devices :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_host_devices"
  f_scl_audio_host_devices :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_audio_hosts"
  f_scl_audio_hosts :: Ptr (CInt) -> Ptr (Ptr (CChar)) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_default_audio_host"
  f_scl_default_audio_host :: Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_audio_device_name"
  f_scl_audio_device_name :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_audio_device_host"
  f_scl_audio_device_host :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_audio_device_max_input_channels"
  f_scl_audio_device_max_input_channels :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_device_max_output_channels"
  f_scl_audio_device_max_output_channels :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_device_default_low_input_latency"
  f_scl_audio_device_default_low_input_latency :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_device_default_high_input_latency"
  f_scl_audio_device_default_high_input_latency :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_device_default_low_output_latency"
  f_scl_audio_device_default_low_output_latency :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_device_default_high_output_latency"
  f_scl_audio_device_default_high_output_latency :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_audio_device_default_sample_rate"
  f_scl_audio_device_default_sample_rate :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_default_audio_input_device"
  f_scl_default_audio_input_device :: Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_default_audio_output_device"
  f_scl_default_audio_output_device :: Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_midi_device_name"
  f_scl_midi_device_name :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_midi_device_host_name"
  f_scl_midi_device_host_name :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_midi_device_has_input"
  f_scl_midi_device_has_input :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_midi_device_has_output"
  f_scl_midi_device_has_output :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_default_midi_input_device"
  f_scl_default_midi_input_device :: Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_default_midi_output_device"
  f_scl_default_midi_output_device :: Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_midi_devices"
  f_scl_midi_devices :: Ptr (CInt) -> Ptr (Ptr (CChar)) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_processor_controls"
  f_scl_processor_controls :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_processor_num_inputs"
  f_scl_processor_num_inputs :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_processor_num_outputs"
  f_scl_processor_num_outputs :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_sequence"
  f_scl_sequence :: Ptr (Ptr (CChar)) -> CInt -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_parallel"
  f_scl_parallel :: Ptr (Ptr (CChar)) -> CInt -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_load_fluidsynth"
  f_scl_load_fluidsynth :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_stream_sample_rate"
  f_scl_stream_sample_rate :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_stream_audio_buffer_size"
  f_scl_stream_audio_buffer_size :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_stream_running"
  f_scl_stream_running :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_stream_start"
  f_scl_stream_start :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_stream_stop"
  f_scl_stream_stop :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_stream_abort"
  f_scl_stream_abort :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_stream_set_error_handler"
  f_scl_stream_set_error_handler :: Ptr (CChar) -> FunPtr (CInt -> Ptr (CChar) -> IO (())) -> IO (())
foreign import ccall "wrapper"
  w_scl_stream_set_error_handler_1 :: (CInt -> Ptr (CChar) -> IO (())) -> IO (FunPtr (CInt -> Ptr (CChar) -> IO (())))

--

foreign import ccall "static hs_audioengine.h scl_default_device_stream_options"
  f_scl_default_device_stream_options :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_get_sample_rate"
  f_scl_device_stream_options_get_sample_rate :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_get_audio_buffer_size"
  f_scl_device_stream_options_get_audio_buffer_size :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_get_audio_latency"
  f_scl_device_stream_options_get_audio_latency :: Ptr (CChar) -> IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_get_midi_latency"
  f_scl_device_stream_options_get_midi_latency :: Ptr (CChar) -> IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_is_non_blocking"
  f_scl_device_stream_options_is_non_blocking :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_is_exclusive_mode"
  f_scl_device_stream_options_is_exclusive_mode :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_set_sample_rate"
  f_scl_device_stream_options_set_sample_rate :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_set_audio_buffer_size"
  f_scl_device_stream_options_set_audio_buffer_size :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_set_audio_latency"
  f_scl_device_stream_options_set_audio_latency :: Ptr (CChar) -> CDouble -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_set_midi_latency"
  f_scl_device_stream_options_set_midi_latency :: Ptr (CChar) -> CDouble -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_set_non_blocking"
  f_scl_device_stream_options_set_non_blocking :: Ptr (CChar) -> CInt -> IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_device_stream_options_set_exclusive_mode"
  f_scl_device_stream_options_set_exclusive_mode :: Ptr (CChar) -> CInt -> IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_open_device_stream"
  f_scl_open_device_stream :: Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_interrupt_future"
  f_scl_interrupt_future :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_interrupt_future_group"
  f_scl_interrupt_future_group :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_new_future_group"
  f_scl_new_future_group :: CInt -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_future_group_interruption_mode"
  f_scl_future_group_interruption_mode :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_default_schedule_options"
  f_scl_default_schedule_options :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_get_unit"
  f_scl_schedule_options_get_unit :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_get_groups"
  f_scl_schedule_options_get_groups :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_get_repeats"
  f_scl_schedule_options_get_repeats :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_get_interval"
  f_scl_schedule_options_get_interval :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_set_unit"
  f_scl_schedule_options_set_unit :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_set_groups"
  f_scl_schedule_options_set_groups :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_set_repeats"
  f_scl_schedule_options_set_repeats :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_schedule_options_set_interval"
  f_scl_schedule_options_set_interval :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_schedule_now"
  f_scl_schedule_now :: Ptr (CChar) -> FunPtr (CInt -> IO (())) -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))
foreign import ccall "wrapper"
  w_scl_schedule_now_1 :: (CInt -> IO (())) -> IO (FunPtr (CInt -> IO (())))

--

foreign import ccall "static hs_audioengine.h scl_schedule_later"
  f_scl_schedule_later :: Ptr (CChar) -> CInt -> FunPtr (CInt -> IO (())) -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))
foreign import ccall "wrapper"
  w_scl_schedule_later_1 :: (CInt -> IO (())) -> IO (FunPtr (CInt -> IO (())))

--

foreign import ccall "static hs_audioengine.h scl_schedule_at"
  f_scl_schedule_at :: Ptr (CChar) -> CInt -> FunPtr (CInt -> IO (())) -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))
foreign import ccall "wrapper"
  w_scl_schedule_at_1 :: (CInt -> IO (())) -> IO (FunPtr (CInt -> IO (())))

--

foreign import ccall "static hs_audioengine.h scl_default_send_options"
  f_scl_default_send_options :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_send_options_get_kind"
  f_scl_send_options_get_kind :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_send_options_get_processors"
  f_scl_send_options_get_processors :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_send_options_get_devices"
  f_scl_send_options_get_devices :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_send_options_get_channels"
  f_scl_send_options_get_channels :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (CInt))

--

foreign import ccall "static hs_audioengine.h scl_send_options_set_kind"
  f_scl_send_options_set_kind :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_send_options_set_processors"
  f_scl_send_options_set_processors :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_send_options_set_devices"
  f_scl_send_options_set_devices :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_send_options_set_channels"
  f_scl_send_options_set_channels :: Ptr (CChar) -> Ptr (CInt) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_default_receive_options"
  f_scl_default_receive_options :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_receive_options_get_kind"
  f_scl_receive_options_get_kind :: Ptr (CChar) -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_receive_options_get_processors"
  f_scl_receive_options_get_processors :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_receive_options_get_devices"
  f_scl_receive_options_get_devices :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_receive_options_get_channels"
  f_scl_receive_options_get_channels :: Ptr (CChar) -> Ptr (CInt) -> IO (Ptr (CInt))

--

foreign import ccall "static hs_audioengine.h scl_receive_options_set_kind"
  f_scl_receive_options_set_kind :: Ptr (CChar) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_receive_options_set_processors"
  f_scl_receive_options_set_processors :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_receive_options_set_devices"
  f_scl_receive_options_set_devices :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_receive_options_set_channels"
  f_scl_receive_options_set_channels :: Ptr (CChar) -> Ptr (CInt) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_send_now"
  f_scl_send_now :: Ptr (CChar) -> Ptr (Ptr (CChar)) -> CInt -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_send_later"
  f_scl_send_later :: Ptr (CChar) -> CInt -> Ptr (Ptr (CChar)) -> CInt -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_send_at"
  f_scl_send_at :: Ptr (CChar) -> CInt -> Ptr (Ptr (CChar)) -> CInt -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_receive"
  f_scl_receive :: Ptr (CChar) -> FunPtr (CInt -> Ptr (Ptr (CChar)) -> CInt -> IO (())) -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))
foreign import ccall "wrapper"
  w_scl_receive_1 :: (CInt -> Ptr (Ptr (CChar)) -> CInt -> IO (())) -> IO (FunPtr (CInt -> Ptr (Ptr (CChar)) -> CInt -> IO (())))

--

foreign import ccall "static hs_audioengine.h scl_receive_buffered"
  f_scl_receive_buffered :: Ptr (CChar) -> FunPtr (CInt -> Ptr (Ptr (Ptr (CChar))) -> Ptr (CInt) -> CInt -> IO (())) -> Ptr (CChar) -> Ptr (Ptr (CChar)) -> IO (Ptr (CChar))
foreign import ccall "wrapper"
  w_scl_receive_buffered_1 :: (CInt -> Ptr (Ptr (Ptr (CChar))) -> Ptr (CInt) -> CInt -> IO (())) -> IO (FunPtr (CInt -> Ptr (Ptr (Ptr (CChar))) -> Ptr (CInt) -> CInt -> IO (())))

--

foreign import ccall "static hs_audioengine.h scl_test_nothing"
  f_scl_test_nothing :: IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_error"
  f_scl_test_error :: CInt -> Ptr (Ptr (CChar)) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_two_errors"
  f_scl_test_two_errors :: CInt -> Ptr (Ptr (CChar)) -> Ptr (Ptr (CChar)) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_pass_int"
  f_scl_test_pass_int :: CInt -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_test_pass_float"
  f_scl_test_pass_float :: CFloat -> IO (CFloat)

--

foreign import ccall "static hs_audioengine.h scl_test_pass_double"
  f_scl_test_pass_double :: CDouble -> IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_test_pass_enum"
  f_scl_test_pass_enum :: CInt -> IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_test_pass_string"
  f_scl_test_pass_string :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_test_pass_atom"
  f_scl_test_pass_atom :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_test_pass_object"
  f_scl_test_pass_object :: Ptr (CChar) -> IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_test_pass_list_int"
  f_scl_test_pass_list_int :: Ptr (CInt) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_pass_list_string"
  f_scl_test_pass_list_string :: Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_pass_list_object"
  f_scl_test_pass_list_object :: Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_pass_list_atom"
  f_scl_test_pass_list_atom :: Ptr (Ptr (CChar)) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_pass_list_list_int"
  f_scl_test_pass_list_list_int :: Ptr (Ptr (CInt)) -> Ptr (CInt) -> CInt -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_test_return_int"
  f_scl_test_return_int :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_test_return_float"
  f_scl_test_return_float :: IO (CFloat)

--

foreign import ccall "static hs_audioengine.h scl_test_return_double"
  f_scl_test_return_double :: IO (CDouble)

--

foreign import ccall "static hs_audioengine.h scl_test_return_enum"
  f_scl_test_return_enum :: IO (CInt)

--

foreign import ccall "static hs_audioengine.h scl_test_return_string"
  f_scl_test_return_string :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_test_return_atom"
  f_scl_test_return_atom :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_test_return_object"
  f_scl_test_return_object :: IO (Ptr (CChar))

--

foreign import ccall "static hs_audioengine.h scl_test_return_list_int"
  f_scl_test_return_list_int :: Ptr (CInt) -> IO (Ptr (CInt))

--

foreign import ccall "static hs_audioengine.h scl_test_return_list_string"
  f_scl_test_return_list_string :: Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_test_return_list_object"
  f_scl_test_return_list_object :: Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_test_return_list_atom"
  f_scl_test_return_list_atom :: Ptr (CInt) -> IO (Ptr (Ptr (CChar)))

--

foreign import ccall "static hs_audioengine.h scl_test_return_list_list_int"
  f_scl_test_return_list_list_int :: Ptr (Ptr (CInt)) -> Ptr (CInt) -> IO (Ptr (Ptr (CInt)))

--

foreign import ccall "static hs_audioengine.h scl_free_error"
  f_scl_free_error :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_portaudio_error"
  f_scl_free_portaudio_error :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_portmidi_error"
  f_scl_free_portmidi_error :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_audio_host"
  f_scl_free_audio_host :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_audio_device"
  f_scl_free_audio_device :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_midi_device"
  f_scl_free_midi_device :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_future"
  f_scl_free_future :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_stream"
  f_scl_free_stream :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_processor"
  f_scl_free_processor :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_future_group"
  f_scl_free_future_group :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_atom"
  f_scl_free_atom :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_schedule_options"
  f_scl_free_schedule_options :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_send_options"
  f_scl_free_send_options :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_receive_options"
  f_scl_free_receive_options :: Ptr (CChar) -> IO (())

--

foreign import ccall "static hs_audioengine.h scl_free_array"
  f_scl_free_array :: Ptr (Ptr (CChar)) -> IO (())

--


