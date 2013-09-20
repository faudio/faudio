
#ifndef _FA_RATIO
#define _FA_RATIO

#include <fa/std.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaRatio Ratio
    @{
    */

typedef int32_t fa_ratio_num_t;
typedef int32_t fa_ratio_denom_t;
typedef struct _fa_ratio_t * fa_ratio_t;
fa_ratio_t fa_ratio_create(fa_ratio_num_t, fa_ratio_denom_t);
fa_ratio_num_t fa_ratio_num(fa_ratio_t);
fa_ratio_denom_t fa_ratio_denom(fa_ratio_t);
void fa_ratio_match(fa_ratio_t,
                    fa_ratio_num_t *,
                    fa_ratio_denom_t *);
fa_ratio_t fa_ratio_copy(fa_ratio_t);
void fa_ratio_destroy(fa_ratio_t);
fa_ratio_t fa_ratio_add(fa_ratio_t, fa_ratio_t);
fa_ratio_t fa_ratio_subtract(fa_ratio_t, fa_ratio_t);
fa_ratio_t fa_ratio_multiply(fa_ratio_t, fa_ratio_t);
fa_ratio_t fa_ratio_divide(fa_ratio_t, fa_ratio_t);
fa_ratio_t fa_ratio_succ(fa_ratio_t);
fa_ratio_t fa_ratio_pred(fa_ratio_t);
fa_ratio_t fa_ratio_negate(fa_ratio_t);
fa_ratio_t fa_ratio_recip(fa_ratio_t);
fa_ratio_t fa_ratio_normalize(fa_ratio_t);
void fa_ratio_to_mixed(fa_ratio_t, fa_ratio_num_t *, fa_ratio_t *);

/** @}
    @}
    */

#endif // _FA_RATIO

