
#ifndef _FAE_RATIO
#define _FAE_RATIO

#include <fae/std.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeRatio Ratio
    @{
    */

typedef int32_t fae_ratio_num_t;
typedef int32_t fae_ratio_denom_t;
typedef struct _fae_ratio_t * fae_ratio_t;
fae_ratio_t fae_ratio_create(fae_ratio_num_t, fae_ratio_denom_t);
fae_ratio_num_t fae_ratio_num(fae_ratio_t);
fae_ratio_denom_t fae_ratio_denom(fae_ratio_t);
void fae_ratio_match(fae_ratio_t,
                     fae_ratio_num_t *,
                     fae_ratio_denom_t *);
fae_ratio_t fae_ratio_copy(fae_ratio_t);
void fae_ratio_destroy(fae_ratio_t);
fae_ratio_t fae_ratio_add(fae_ratio_t, fae_ratio_t);
fae_ratio_t fae_ratio_subtract(fae_ratio_t, fae_ratio_t);
fae_ratio_t fae_ratio_multiply(fae_ratio_t, fae_ratio_t);
fae_ratio_t fae_ratio_divide(fae_ratio_t, fae_ratio_t);
fae_ratio_t fae_ratio_succ(fae_ratio_t);
fae_ratio_t fae_ratio_pred(fae_ratio_t);
fae_ratio_t fae_ratio_negate(fae_ratio_t);
fae_ratio_t fae_ratio_recip(fae_ratio_t);
fae_ratio_t fae_ratio_normalize(fae_ratio_t);
void fae_ratio_to_mixed(fae_ratio_t,
                        fae_ratio_num_t *,
                        fae_ratio_t *);

/** @}
    @}
    */

#endif // _FAE_RATIO

