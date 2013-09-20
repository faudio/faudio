
#ifndef _FA_AUDIO
#define _FA_AUDIO

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/device.h>
#include <fa/error.h>
#include <fa/type.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaAudio Audio
    @{
    */

typedef struct _fa_audio_session_t * fa_audio_session_t;
typedef struct _fa_audio_device_t * fa_audio_device_t;
typedef struct _fa_audio_stream_t * fa_audio_stream_t;
typedef fa_audio_session_t (* fa_audio_session_callback_t)(fa_ptr_t,
                                                           fa_audio_session_t);
typedef fa_audio_stream_t (* fa_audio_stream_callback_t)(fa_ptr_t,
                                                         fa_audio_stream_t);
typedef fa_nullary_t fa_audio_status_callback_t;
fa_audio_session_t fa_audio_begin_session();
void fa_audio_end_session(fa_audio_session_t);
void fa_audio_with_session(fa_audio_session_callback_t,
                           fa_ptr_t,
                           fa_error_callback_t,
                           fa_ptr_t);
fa_list_t fa_audio_all(fa_audio_session_t);
fa_pair_t fa_audio_default(fa_audio_session_t);
fa_audio_device_t fa_audio_default_input(fa_audio_session_t);
fa_audio_device_t fa_audio_default_output(fa_audio_session_t);
void fa_audio_set_status_callback(fa_audio_status_callback_t,
                                  fa_ptr_t,
                                  fa_audio_session_t);
fa_string_t fa_audio_name(fa_audio_device_t);
fa_string_t fa_audio_host_name(fa_audio_device_t);
bool fa_audio_has_input(fa_audio_device_t);
bool fa_audio_has_output(fa_audio_device_t);
fa_type_t fa_audio_input_type(fa_audio_device_t);
fa_type_t fa_audio_output_type(fa_audio_device_t);
fa_audio_stream_t fa_audio_open_stream(fa_audio_device_t,
                                       fa_ptr_t,
                                       fa_audio_device_t);
void fa_audio_close_stream(fa_audio_stream_t);
void fa_audio_with_stream(fa_audio_device_t,
                          fa_ptr_t,
                          fa_audio_device_t,
                          fa_audio_stream_callback_t,
                          fa_ptr_t,
                          fa_error_callback_t,
                          fa_ptr_t);

/** @}
    @}
    */

#endif // _FA_AUDIO

