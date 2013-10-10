
#ifndef _FA_CLOCK
#define _FA_CLOCK

#include <fa.h>
#include <fa/time.h>

/** @addtogroup FaClock

    Provides various clocks.
        
    A clock is (not surprisingly) a provider of time.

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
        } fa_clock_clock_interface_t;


typedef struct _fa_clock_t * fa_clock_t;


fa_time_t fa_clock_time(fa_clock_t);


fa_time_milliseconds_t fa_clock_milliseconds(fa_clock_t);

/** @}
    @}
    */

#endif // _FA_CLOCK

