
#ifndef _DOREMIR_AUDIOENGINE
#define _DOREMIR_AUDIOENGINE

#include <doremir/atomic.h>
#include <doremir/atomic/queue.h>
#include <doremir/atomic/ring_buffer.h>
#include <doremir/error.h>
#include <doremir/device/audio.h>
#include <doremir/device/midi.h>
#include <doremir/device/file.h>
#include <doremir/message.h>
#include <doremir/midi.h>
#include <doremir/processor.h>
#include <doremir/scheduler.h>
#include <doremir/thread.h>
#include <doremir/time.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAudioEngine AudioEngine
    @{
    */

void doremir_audio_engine_initialize();
void doremir_audio_engine_terminate();
typedef void (* doremir_audio_engine_log_func_t)(doremir_ptr_t,
                                                 doremir_time_system_t,
                                                 doremir_error_t);
void doremir_audio_engine_set_log_file(doremir_string_file_path_t);
void doremir_audio_engine_set_log_std();
void doremir_audio_engine_set_log(doremir_audio_engine_log_func_t,
                                  doremir_ptr_t);
void doremir_audio_engine_log(doremir_ptr_t, doremir_error_t);
void doremir_audio_engine_log_info(doremir_string_t);
void doremir_audio_engine_log_warning(doremir_string_t);
void doremir_audio_engine_log_error(doremir_string_t);
void doremir_audio_engine_log_info_from(doremir_string_t,
                                        doremir_string_t);
void doremir_audio_engine_log_warning_from(doremir_string_t,
                                           doremir_string_t);
void doremir_audio_engine_log_error_from(doremir_string_t,
                                         doremir_string_t);

/** @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE

