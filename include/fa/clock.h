
#ifndef _FA_CLOCK
#define _FA_CLOCK

#include <fa.h>
#include <fa/time.h>

/** @addtogroup FaClock

    Provides various clocks.
        
    A clock is (not surprisingly) a provider of time.

    @par Literals
    - `hms(0,1,30)`
    - `days(2)`
    - `hours(2)`
    - `minutes(3)`
    - `seconds(4)`
    - `milliseconds(3501)`
    - `microseconds(3500421)`
    - `divisions(1, 3)`
    - `fa_add(minutes(1), milliseconds(100))`
    - `fa_subtract(hours(0), seconds(2))`

    @par Implements
    - fa_equal_t
    - fa_order_t
    - fa_string_show_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t
    - fa_number_t

    @since
        2.3
 
    @defgroup Fa Fa
    @{
    @defgroup FaClock Clock
    @{
    */


typedef struct {
            fa_time_t (* time)(fa_ptr_t);
            double (* tick_rate)(fa_ptr_t);
            int64_t (* ticks)(fa_ptr_t);
        } fa_clock_clock_interface_t;


typedef struct _fa_clock_t * fa_clock_t;


fa_time_t fa_clock_time(fa_clock_t);


double fa_clock_tick_rate(fa_clock_t);


int64_t fa_clock_ticks(fa_clock_t);


fa_clock_t fa_clock_get_system_clock();


fa_clock_t fa_clock_get_system_prec_clock();


fa_clock_t fa_clock_get_cpu_clock();

/** @}
    @}
    */

#endif // _FA_CLOCK

