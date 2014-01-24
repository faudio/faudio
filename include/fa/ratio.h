
#ifndef _FA_RATIO
#define _FA_RATIO

#include <fa/std.h>

/** @addtogroup FaRatio
 
    Rational numbers.

    @par Literals
    - `ratio(1, 2)`
    - `ratio(0, 1)`
    - `ratio(-1, 2)`

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
    @defgroup FaRatio Ratio
    @{
    */


typedef int32_t fa_ratio_num_t;


typedef int32_t fa_ratio_denom_t;

/** A rational number.
*/
typedef struct _fa_ratio_t * fa_ratio_t;

/** Create a rational number.
*/
fa_ratio_t fa_ratio_create(fa_ratio_num_t num,
                           fa_ratio_denom_t denom);

/** Return the numerator of the given rational number.
*/
fa_ratio_num_t fa_ratio_num(fa_ratio_t ratio);

/** Return the denominator of the given rational number.
*/
fa_ratio_denom_t fa_ratio_denom(fa_ratio_t ratio);

/** Destruct the given rational number, writing its numerator
    and denominator to the given locations.
*/
void fa_ratio_match(fa_ratio_t ratio,
                    fa_ratio_num_t *,
                    fa_ratio_denom_t *);

/** Copy a rational number.
*/
fa_ratio_t fa_ratio_copy(fa_ratio_t ratio);

/** Destroy a rational number.
*/
void fa_ratio_destroy(fa_ratio_t ratio);

/** Add the given rational numbers.
*/
fa_ratio_t fa_ratio_add(fa_ratio_t ratio, fa_ratio_t ratio_);

/** Subtract the given rational numbers.
*/
fa_ratio_t fa_ratio_subtract(fa_ratio_t ratio, fa_ratio_t ratio_);

/** Multiply the given rational numbers.
*/
fa_ratio_t fa_ratio_multiply(fa_ratio_t ratio, fa_ratio_t ratio_);

/** Divide the given rational numbers.
*/
fa_ratio_t fa_ratio_divide(fa_ratio_t ratio, fa_ratio_t ratio_);

/** Return the successor of the given rational number.
*/
fa_ratio_t fa_ratio_succ(fa_ratio_t ratio);

/** Return the predecessor of the given rational number.
*/
fa_ratio_t fa_ratio_pred(fa_ratio_t ratio);

/** Negate the given rational number.
*/
fa_ratio_t fa_ratio_negate(fa_ratio_t ratio);

/** Invert the given rational number.
*/
fa_ratio_t fa_ratio_recip(fa_ratio_t ratio);

/** Return the absolute value of the given rational number.
*/
fa_ratio_t fa_ratio_absolute(fa_ratio_t ratio);

/** Normalize the given rational number.
*/
fa_ratio_t fa_ratio_normalize(fa_ratio_t ratio);

/** Convert the given rational number to mixed form.

    For example \f$11/3\f$ becomes \f$3+2/3\f$.
*/
void fa_ratio_to_mixed(fa_ratio_t ratio,
                       fa_ratio_num_t *,
                       fa_ratio_t *);

/** @}
    @}
    */

#endif // _FA_RATIO

