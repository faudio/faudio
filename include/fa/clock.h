
#ifndef _FA_CLOCK
#define _FA_CLOCK

#include <fa.h>
#include <fa/time.h>

/** @addtogroup FaClock

    Provides various clocks.
        
    As one might expect, a clock is a provider of time. A clock in *faudio*,
    however does not usually indicate real-world time, but the current time
    of an audio computation.

    The most accurate clocks are those derived directly from an audio stream. See
    @ref fa_audio_stream_clock.

    @par Implements
    - fa_string_show_t (all clock types)
    - fa_destroy_t (all clock types)

    @since
        2.3
 
    @defgroup Fa Fa
    @{
    @defgroup FaClock Clock
    @{
    */


typedef struct {
            fa_time_t (* time)(fa_ptr_t); int64_t (* milliseconds)(fa_ptr_t);
        } fa_clock_interface_t;

/** A value of an unknown type implementing @ref fa_clock_interface.
*/
typedef struct _fa_clock_t * fa_clock_t;

/** Returns the current time according to the given clock.
    @param clock
        Clock to use.
    @return
        The current time.
*/
fa_time_t fa_clock_time(fa_clock_t);

/** Returns the current time in milliseconds according to the given clock.
    @param clock
        Clock to use.
    @return
        The current time in milliseconds.
*/
fa_time_milliseconds_t fa_clock_milliseconds(fa_clock_t);

/** Returns the standard clock.
    @return
        A clock.
*/
fa_clock_t fa_clock_standard();

/** @}
    @}
    */

#endif // _FA_CLOCK

