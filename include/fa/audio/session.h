
#ifndef _FA_AUDIO_SESSION
#define _FA_AUDIO_SESSION

#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>

/** @addtogroup FaAudioSession

    Provides real-time audio.
    
    These device run processors on the input and output
    the underlying system, typically physical audio interfaces. A running audio
    computation is represented by a *stream*. Access to the current device setups
    is provided by *sessions*.

    @par Implements
    - fa_equal_t
    - fa_destroy_t (sessions and streams)
    - fa_string_show_t

    @see
    - @ref Devices
 
    @defgroup Fa Fa
    @{
    @defgroup FaAudio Audio
    @{
    @defgroup FaAudioSession Session
    @{
    */



/** @}
    @}
    @}
    */

#endif // _FA_AUDIO_SESSION

