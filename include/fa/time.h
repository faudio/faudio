
#ifndef _FA_TIME
#define _FA_TIME

#include <fa.h>
#include <fa/ratio.h>
#include <fa/string.h>

/** @addtogroup FaTime

    @addtogroup FaTime

    Aribitrary precision time interval.

    This type uses standard time units, with arbitrary divisions of a second. Allthough
    day is the longest unit, times longer than a day are supported by this type; they are
    simply expressed as days instead of years, weaks and months. Thus no particular
    calendar has to be adopted.

    To *deconstruct* a type, use `fa_time_days`, `fa_time_hours` etc. To *convert* a
    type to a numeric type (such as number of milliseconds), use
    `fa_time_to_milliseconds` etc. For example:
    
        fa_time_minutes(hms(0,4,33))    = 4
        fa_time_seconds(hms(0,4,33))    = 33
        fa_time_to_minutes(hms(0,4,33)) = 4*60 + 33 = 273

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

 
    @defgroup Fa Fa
    @{
    @defgroup FaTime Time
    @{
    */


typedef struct _fa_time_t * fa_time_t;

/** Create a new time interval.
    @param days
        Number of days.
    @param hours
        Number of hours.
    @param minutes
        Number of minutes.
    @param seconds
        Number of seconds (destroyed).
    @return
        A new time value.
*/
fa_time_t fa_time_create(int32_t, int32_t, int32_t, fa_ratio_t);

/**
    Copy the given time interval.
*/
fa_time_t fa_time_copy(fa_time_t);

/**
    Destroy the given time interval.
*/
void fa_time_destroy(fa_time_t);

/**
    Return the number of whole days in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in days modulo one.
*/
int32_t fa_time_days(fa_time_t);

/**
    Return the number of whole hours in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in hours modulo one.
*/
int32_t fa_time_hours(fa_time_t);

/**
    Return the number of whole minutes in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in minutes modulo one.
*/
int32_t fa_time_minutes(fa_time_t);

/**
    Return the number of whole seconds in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in seconds modulo one.
*/
int32_t fa_time_seconds(fa_time_t);

/**
    Return the fractions of a second in this time interval.

    @param time
        Time interval.
    @return
        Rational number, representing the remainder of time in seconds over one.
*/
fa_ratio_t fa_time_divisions(fa_time_t);

/** Print the time as an ISO 8601 duration.

    The ISO represenation use decimal fractions of a second, and may lose precision. For example
    the duration of 1 min 24 1/3 sec would be represented as `P0000-00-00T00:01:24.3333`.
*/
fa_string_t fa_time_to_iso(fa_time_t);

/** Convert the time to seconds.
    This may lose precision.

    @param time
        Time interval.
*/
int32_t fa_time_to_seconds(fa_time_t);


int32_t fa_time_to_milliseconds(fa_time_t);


typedef struct _fa_time_system_t * fa_time_system_t;


typedef struct _fa_time_cpu_t * fa_time_cpu_t;

/** Convert system time to a time interval.
    Generally, system time is seconds since the Unix epoch.
*/
fa_time_t fa_time_from_system(fa_time_system_t);

/** Convert system CPU time to a time interval.
*/
fa_time_t fa_time_from_cpu(fa_time_cpu_t);

/** Get the system time.
*/
fa_time_system_t fa_time_system();

/** Get the system CPU time.
*/
fa_time_cpu_t fa_time_cpu();

/** @}
    @}
    */

#endif // _FA_TIME

