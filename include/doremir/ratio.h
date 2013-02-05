
#ifndef _DOREMIR_RATIO
#define _DOREMIR_RATIO

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirRatio Ratio
    @{
    */

typedef int32_t doremir_ratio_num_t;
typedef int32_t doremir_ratio_denom_t;
typedef struct _doremir_ratio_t * doremir_ratio_t;
doremir_ratio_t doremir_ratio_create(doremir_ratio_num_t,
                                     doremir_ratio_denom_t);
doremir_ratio_num_t doremir_ratio_num(doremir_ratio_t);
doremir_ratio_denom_t doremir_ratio_denom(doremir_ratio_t);
void doremir_ratio_match(doremir_ratio_t,
                         doremir_ratio_num_t *,
                         doremir_ratio_denom_t *);
void doremir_ratio_destroy(doremir_ratio_t);
doremir_ratio_t doremir_ratio_add(doremir_ratio_t,
                                  doremir_ratio_t);
doremir_ratio_t doremir_ratio_subtract(doremir_ratio_t,
                                       doremir_ratio_t);
doremir_ratio_t doremir_ratio_multiply(doremir_ratio_t,
                                       doremir_ratio_t);
doremir_ratio_t doremir_ratio_divide(doremir_ratio_t,
                                     doremir_ratio_t);
doremir_ratio_t doremir_ratio_succ(doremir_ratio_t);
doremir_ratio_t doremir_ratio_pred(doremir_ratio_t);
doremir_ratio_t doremir_ratio_negate(doremir_ratio_t);
doremir_ratio_t doremir_ratio_recip(doremir_ratio_t);

/** @}
    @}
    */

#endif // _DOREMIR_RATIO

