
#include <doremir/audio_engine.h>

void doremir_device_audio_initialize();
void doremir_device_audio_terminate();


static int count = 0;

/** Performs global initialization. 

    This function must be called exactly once before any other function in the library.
    A call to doremir_audio_engine_terminate() will reset the global state so that
    doremir_audio_engine_initialize() may be called again and so on.
 */
void doremir_audio_engine_initialize()
{
    doremir_device_audio_initialize();
}

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
 */
void doremir_audio_engine_terminate()
{
    doremir_device_audio_terminate();
}