
#ifndef _FAE_TIME
#define _FAE_TIME

#include <fae.h>
#include <fae/ratio.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeTime Time
    @{
    */

typedef struct _fae_time_t * fae_time_t;
fae_time_t fae_time_create(int32_t, int32_t, int32_t, fae_ratio_t);
fae_time_t fae_time_copy(fae_time_t);
void fae_time_destroy(fae_time_t);
int32_t fae_time_days(fae_time_t);
int32_t fae_time_hours(fae_time_t);
int32_t fae_time_minutes(fae_time_t);
int32_t fae_time_seconds(fae_time_t);
fae_ratio_t fae_time_divisions(fae_time_t);
fae_string_t fae_time_to_iso(fae_time_t);
int32_t fae_time_to_seconds(fae_time_t);
int32_t fae_time_to_milliseconds(fae_time_t);
typedef struct _fae_time_system_t * fae_time_system_t;
typedef struct _fae_time_cpu_t * fae_time_cpu_t;
fae_time_t fae_time_from_system(fae_time_system_t);
fae_time_t fae_time_from_cpu(fae_time_cpu_t);
fae_time_system_t fae_time_system();
fae_time_cpu_t fae_time_cpu();

/** @}
    @}
    */

#endif // _FAE_TIME

