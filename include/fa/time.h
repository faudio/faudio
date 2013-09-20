
#ifndef _FA_TIME
#define _FA_TIME

#include <fa.h>
#include <fa/ratio.h>
#include <fa/string.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaTime Time
    @{
    */

typedef struct _fa_time_t * fa_time_t;
fa_time_t fa_time_create(int32_t, int32_t, int32_t, fa_ratio_t);
fa_time_t fa_time_copy(fa_time_t);
void fa_time_destroy(fa_time_t);
int32_t fa_time_days(fa_time_t);
int32_t fa_time_hours(fa_time_t);
int32_t fa_time_minutes(fa_time_t);
int32_t fa_time_seconds(fa_time_t);
fa_ratio_t fa_time_divisions(fa_time_t);
fa_string_t fa_time_to_iso(fa_time_t);
int32_t fa_time_to_seconds(fa_time_t);
int32_t fa_time_to_milliseconds(fa_time_t);
typedef struct _fa_time_system_t * fa_time_system_t;
typedef struct _fa_time_cpu_t * fa_time_cpu_t;
fa_time_t fa_time_from_system(fa_time_system_t);
fa_time_t fa_time_from_cpu(fa_time_cpu_t);
fa_time_system_t fa_time_system();
fa_time_cpu_t fa_time_cpu();

/** @}
    @}
    */

#endif // _FA_TIME

