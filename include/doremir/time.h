
#ifndef _DOREMIR_TIME
#define _DOREMIR_TIME

#include <doremir.h>
#include <doremir/ratio.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirTime Time
    @{
    */

typedef struct _doremir_time_t * doremir_time_t;
doremir_time_t doremir_time_create(int32_t,
                                   int32_t,
                                   int32_t,
                                   doremir_ratio_t);
doremir_time_t doremir_time_copy(doremir_time_t);
void doremir_time_destroy(doremir_time_t);
int32_t doremir_time_days(doremir_time_t);
int32_t doremir_time_hours(doremir_time_t);
int32_t doremir_time_minutes(doremir_time_t);
int32_t doremir_time_seconds(doremir_time_t);
doremir_ratio_t doremir_time_divisions(doremir_time_t);
doremir_string_t doremir_time_to_iso(doremir_time_t);
int32_t doremir_time_to_seconds(doremir_time_t);
int32_t doremir_time_to_milliseconds(doremir_time_t);
typedef struct _doremir_time_system_t * doremir_time_system_t;
typedef struct _doremir_time_cpu_t * doremir_time_cpu_t;
doremir_time_t doremir_time_from_system(doremir_time_system_t);
doremir_time_t doremir_time_from_cpu(doremir_time_cpu_t);
doremir_time_system_t doremir_time_system();
doremir_time_cpu_t doremir_time_cpu();
typedef struct {
            doremir_time_t (* time)(doremir_ptr_t);
            double (* tick_rate)(doremir_ptr_t);
            int64_t (* ticks)(doremir_ptr_t);
        } doremir_time_clock_interface_t;
typedef struct _doremir_time_clock_t * doremir_time_clock_t;
doremir_time_t doremir_time_time(doremir_time_clock_t);
double doremir_time_tick_rate(doremir_time_clock_t);
int64_t doremir_time_ticks(doremir_time_clock_t);
doremir_time_clock_t doremir_time_get_system_clock();
doremir_time_clock_t doremir_time_get_c_p_u_clock();

/** @}
    @}
    */

#endif // _DOREMIR_TIME

