
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

typedef enum {
            second, minute, hour, day
        } doremir_time_unit_t;
typedef struct _doremir_time_t * doremir_time_t;
doremir_time_t doremir_time_create(int32_t,
                                   int32_t,
                                   int32_t,
                                   doremir_ratio_t);
doremir_time_t doremir_time_copy(doremir_time_t);
void doremir_time_destroy(doremir_time_t);
doremir_ratio_t doremir_time_divisions(doremir_time_t);
int32_t doremir_time_seconds(doremir_time_t);
int32_t doremir_time_minutes(doremir_time_t);
int32_t doremir_time_hours(doremir_time_t);
int32_t doremir_time_days(doremir_time_t);
doremir_string_t doremir_time_to_iso(doremir_time_t);

/** @}
    @}
    */

#endif // _DOREMIR_TIME

