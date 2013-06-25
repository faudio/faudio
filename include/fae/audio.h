
#ifndef _FAE_AUDIO
#define _FAE_AUDIO

#include <fae/list.h>
#include <fae/pair.h>
#include <fae/device.h>
#include <fae/error.h>
#include <fae/type.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeAudio Audio
    @{
    */

typedef struct _fae_audio_session_t * fae_audio_session_t;
typedef struct _fae_audio_device_t * fae_audio_device_t;
typedef struct _fae_audio_stream_t * fae_audio_stream_t;
typedef fae_audio_session_t (* fae_audio_session_callback_t)(fae_ptr_t,
                                                             fae_audio_session_t);
typedef fae_audio_stream_t (* fae_audio_stream_callback_t)(fae_ptr_t,
                                                           fae_audio_stream_t);
typedef fae_nullary_t fae_audio_status_callback_t;
fae_audio_session_t fae_audio_begin_session();
void fae_audio_end_session(fae_audio_session_t);
void fae_audio_with_session(fae_audio_session_callback_t,
                            fae_ptr_t,
                            fae_error_callback_t,
                            fae_ptr_t);
fae_list_t fae_audio_all(fae_audio_session_t);
fae_pair_t fae_audio_default(fae_audio_session_t);
fae_audio_device_t fae_audio_default_input(fae_audio_session_t);
fae_audio_device_t fae_audio_default_output(fae_audio_session_t);
void fae_audio_set_status_callback(fae_audio_status_callback_t,
                                   fae_ptr_t,
                                   fae_audio_session_t);
fae_string_t fae_audio_name(fae_audio_device_t);
fae_string_t fae_audio_host_name(fae_audio_device_t);
bool fae_audio_has_input(fae_audio_device_t);
bool fae_audio_has_output(fae_audio_device_t);
fae_type_t fae_audio_input_type(fae_audio_device_t);
fae_type_t fae_audio_output_type(fae_audio_device_t);
fae_audio_stream_t fae_audio_open_stream(fae_audio_device_t,
                                         fae_ptr_t,
                                         fae_audio_device_t);
void fae_audio_close_stream(fae_audio_stream_t);
void fae_audio_with_stream(fae_audio_device_t,
                           fae_ptr_t,
                           fae_audio_device_t,
                           fae_audio_stream_callback_t,
                           fae_ptr_t,
                           fae_error_callback_t,
                           fae_ptr_t);

/** @}
    @}
    */

#endif // _FAE_AUDIO

