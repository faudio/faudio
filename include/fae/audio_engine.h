
#ifndef _FAE_AUDIOENGINE
#define _FAE_AUDIOENGINE

#include <fae/atomic.h>
#include <fae/atomic/queue.h>
#include <fae/atomic/stack.h>
#include <fae/atomic/ring_buffer.h>
#include <fae/system.h>
#include <fae/error.h>
#include <fae/event.h>
#include <fae/graph.h>
#include <fae/device/audio.h>
#include <fae/device/midi.h>
#include <fae/device/file.h>
#include <fae/message.h>
#include <fae/midi.h>
#include <fae/processor.h>
#include <fae/plot.h>
#include <fae/scheduler.h>
#include <fae/thread.h>
#include <fae/time.h>
#include <fae/priority_queue.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeAudioEngine AudioEngine
    @{
    */

fae_list_t fae_audio_engine_version();
fae_string_t fae_audio_engine_version_string();
void fae_audio_engine_initialize();
void fae_audio_engine_terminate();
typedef void (* fae_audio_engine_log_func_t)(fae_ptr_t,
                                             fae_time_system_t,
                                             fae_error_t);
void fae_audio_engine_set_log_file(fae_string_file_path_t);
void fae_audio_engine_set_log_std();
void fae_audio_engine_set_log(fae_audio_engine_log_func_t,
                              fae_ptr_t);
void fae_audio_engine_log(fae_ptr_t, fae_error_t);
void fae_audio_engine_log_info(fae_string_t);
void fae_audio_engine_log_warning(fae_string_t);
void fae_audio_engine_log_error(fae_string_t);
void fae_audio_engine_log_info_from(fae_string_t, fae_string_t);
void fae_audio_engine_log_warning_from(fae_string_t, fae_string_t);
void fae_audio_engine_log_error_from(fae_string_t, fae_string_t);
void fae_audio_engine_dlog(fae_ptr_t, fae_error_t);
void fae_audio_engine_dlog_info(fae_string_t);

/** @}
    @}
    */

#endif // _FAE_AUDIOENGINE

