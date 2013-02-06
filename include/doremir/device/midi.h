
#ifndef _DOREMIR_DEVICE_MIDI
#define _DOREMIR_DEVICE_MIDI

#include <doremir/list.h>
#include <doremir/pair.h>
#include <doremir/device.h>
#include <doremir/error.h>
#include <doremir/event.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDevice Device
    @{
    @defgroup DoremirDeviceMidi Midi
    @{
    */

typedef struct _doremir_device_midi_session_t * doremir_device_midi_session_t;
typedef struct _doremir_device_midi_stream_t * doremir_device_midi_stream_t;
typedef doremir_device_midi_session_t (* doremir_device_midi_session_callback_t)(doremir_ptr_t,
                                                                                 doremir_device_midi_session_t);
typedef doremir_device_midi_stream_t (* doremir_device_midi_stream_callback_t)(doremir_ptr_t,
                                                                               doremir_device_midi_stream_t);
typedef doremir_nullary_t doremir_device_midi_status_callback_t;
doremir_device_midi_session_t doremir_device_midi_begin_session();
void doremir_device_midi_end_session(doremir_device_midi_session_t);
void doremir_device_midi_with_session(doremir_device_midi_session_callback_t,
                                      doremir_ptr_t,
                                      doremir_error_callback_t,
                                      doremir_ptr_t);
doremir_list_t doremir_device_midi_all(doremir_device_midi_session_t);
doremir_pair_t doremir_device_midi_default(doremir_device_midi_session_t);
doremir_device_midi_t doremir_device_midi_default_input(doremir_device_midi_session_t);
doremir_device_midi_t doremir_device_midi_default_output(doremir_device_midi_session_t);
doremir_string_t doremir_device_midi_name(doremir_device_midi_t);
doremir_string_t doremir_device_midi_host_name(doremir_device_midi_t);
bool doremir_device_midi_has_input(doremir_device_midi_t);
bool doremir_device_midi_has_output(doremir_device_midi_t);
doremir_device_midi_stream_t doremir_device_midi_open_stream(doremir_device_midi_t);
void doremir_device_midi_close_stream(doremir_device_midi_stream_t);
void doremir_device_midi_with_stream(doremir_device_midi_t,
                                     doremir_device_midi_stream_callback_t,
                                     doremir_ptr_t,
                                     doremir_error_callback_t,
                                     doremir_ptr_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_DEVICE_MIDI

