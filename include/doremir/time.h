
#ifndef _DOREMIR_TIME
#define _DOREMIR_TIME

#include <doremir.h>
#include <doremir/ratio.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirTime Time
    @{
    */

typedef enum {
            second, minute, hour, day
        } doremir_time_unit_t;
typedef struct {
            doremir_ratio_t value; doremir_time_unit_t unit;
        } doremir_time_t;

/** @}
    @}
    */

#endif // _DOREMIR_TIME

