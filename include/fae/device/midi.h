
#ifndef _FAE_DEVICE_MIDI
#define _FAE_DEVICE_MIDI

#include <fae/list.h>
#include <fae/pair.h>
#include <fae/device.h>
#include <fae/error.h>
#include <fae/event.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeDevice Device
    @{
    @defgroup FaeDeviceMidi Midi
    @{
    */

typedef struct _fae_device_midi_session_t * fae_device_midi_session_t;
typedef struct _fae_device_midi_stream_t * fae_device_midi_stream_t;
typedef fae_device_midi_session_t (* fae_device_midi_session_callback_t)(fae_ptr_t,
                                                                         fae_device_midi_session_t);
typedef fae_device_midi_stream_t (* fae_device_midi_stream_callback_t)(fae_ptr_t,
                                                                       fae_device_midi_stream_t);
typedef fae_nullary_t fae_device_midi_status_callback_t;
fae_device_midi_session_t fae_device_midi_begin_session();
void fae_device_midi_end_session(fae_device_midi_session_t);
void fae_device_midi_with_session(fae_device_midi_session_callback_t,
                                  fae_ptr_t,
                                  fae_error_callback_t,
                                  fae_ptr_t);
fae_list_t fae_device_midi_all(fae_device_midi_session_t);
fae_pair_t fae_device_midi_default(fae_device_midi_session_t);
fae_device_midi_t fae_device_midi_default_input(fae_device_midi_session_t);
fae_device_midi_t fae_device_midi_default_output(fae_device_midi_session_t);
void fae_device_midi_set_status_callback(fae_device_midi_status_callback_t,
                                         fae_ptr_t,
                                         fae_device_midi_session_t);
fae_string_t fae_device_midi_name(fae_device_midi_t);
fae_string_t fae_device_midi_host_name(fae_device_midi_t);
bool fae_device_midi_has_input(fae_device_midi_t);
bool fae_device_midi_has_output(fae_device_midi_t);
fae_device_midi_stream_t fae_device_midi_open_stream(fae_device_midi_t);
void fae_device_midi_close_stream(fae_device_midi_stream_t);
void fae_device_midi_with_stream(fae_device_midi_t,
                                 fae_device_midi_stream_callback_t,
                                 fae_ptr_t,
                                 fae_error_callback_t,
                                 fae_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FAE_DEVICE_MIDI

