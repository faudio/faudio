
#ifndef _DOREMIR_AUDIOENGINE
#define _DOREMIR_AUDIOENGINE

#include <doremir/atomic.h>
#include <doremir/atomic/queue.h>
#include <doremir/atomic/ring_buffer.h>
#include <doremir/thread.h>
#include <doremir/device/audio.h>
#include <doremir/device/midi.h>
#include <doremir/device/file.h>
#include <doremir/processor.h>
#include <doremir/message.h>
#include <doremir/scheduler.h>
#include <doremir/midi.h>
#include <doremir/time.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAudioEngine AudioEngine
    @{
    */

void doremir_audio_engine_initialize();
void doremir_audio_engine_terminate();
void doremir_audio_engine_set_log_file(doremir_string_file_path_t);
void doremir_audio_engine_set_log_std();

/** @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE

