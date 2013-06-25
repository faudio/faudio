
#ifndef _FAE_MIDI
#define _FAE_MIDI

#include <fae/list.h>
#include <fae/pair.h>
#include <fae/device.h>
#include <fae/error.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeMidi Midi
    @{
    */

typedef struct _fae_midi_device_t * fae_midi_device_t;
typedef struct _fae_midi_session_t * fae_midi_session_t;
typedef struct _fae_midi_stream_t * fae_midi_stream_t;
typedef fae_midi_session_t (* fae_midi_session_callback_t)(fae_ptr_t,
                                                           fae_midi_session_t);
typedef fae_midi_stream_t (* fae_midi_stream_callback_t)(fae_ptr_t,
                                                         fae_midi_stream_t);
typedef fae_nullary_t fae_midi_status_callback_t;
fae_midi_session_t fae_midi_begin_session();
void fae_midi_end_session(fae_midi_session_t);
void fae_midi_with_session(fae_midi_session_callback_t,
                           fae_ptr_t,
                           fae_error_callback_t,
                           fae_ptr_t);
fae_list_t fae_midi_all(fae_midi_session_t);
fae_pair_t fae_midi_default(fae_midi_session_t);
fae_midi_device_t fae_midi_default_input(fae_midi_session_t);
fae_midi_device_t fae_midi_default_output(fae_midi_session_t);
void fae_midi_set_status_callback(fae_midi_status_callback_t,
                                  fae_ptr_t,
                                  fae_midi_session_t);
fae_string_t fae_midi_name(fae_midi_device_t);
fae_string_t fae_midi_host_name(fae_midi_device_t);
bool fae_midi_has_input(fae_midi_device_t);
bool fae_midi_has_output(fae_midi_device_t);
fae_midi_stream_t fae_midi_open_stream(fae_midi_device_t);
void fae_midi_close_stream(fae_midi_stream_t);
void fae_midi_with_stream(fae_midi_device_t,
                          fae_midi_stream_callback_t,
                          fae_ptr_t,
                          fae_error_callback_t,
                          fae_ptr_t);

/** @}
    @}
    */

#endif // _FAE_MIDI

