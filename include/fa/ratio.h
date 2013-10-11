
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


typedef int32_t fa_ratio_numerator_t;


typedef int32_t fa_ratio_denominator_t;

/** A rational number.
*/
typedef struct _fa_ratio_t * fa_ratio_t;

/** Create a rational number.
*/
fa_ratio_t fa_ratio_create(fa_ratio_numerator_t,
                           fa_ratio_denominator_t);

/** Return the numerator of the given rational number.
*/
fa_ratio_numerator_t fa_ratio_num(fa_ratio_t);

/** Return the denominator of the given rational number.
*/
fa_ratio_denominator_t fa_ratio_denom(fa_ratio_t);

/** Destruct the given rational number, writing its numerator
    and denominator to the given locations.
*/
void fa_ratio_match(fa_ratio_t,
                    fa_ratio_numerator_t *,
                    fa_ratio_denominator_t *);

/** Copy a rational number.
*/
fa_ratio_t fa_ratio_copy(fa_ratio_t);

/** Destroy a rational number.
*/
void fa_ratio_destroy(fa_ratio_t);

/** Add the given rational numbers.
*/
fa_ratio_t fa_ratio_add(fa_ratio_t, fa_ratio_t);

/** Subtract the given rational numbers.
*/
fa_ratio_t fa_ratio_subtract(fa_ratio_t, fa_ratio_t);

/** Multiply the given rational numbers.
*/
fa_ratio_t fa_ratio_multiply(fa_ratio_t, fa_ratio_t);

/** Divide the given rational numbers.
*/
fa_ratio_t fa_ratio_divide(fa_ratio_t, fa_ratio_t);

/** Return the successor of the given rational number.
*/
fa_ratio_t fa_ratio_succ(fa_ratio_t);

/** Return the predecessor of the given rational number.
*/
fa_ratio_t fa_ratio_pred(fa_ratio_t);

/** Negate the given rational number.
*/
fa_ratio_t fa_ratio_negate(fa_ratio_t);

/** Invert the given rational number.
*/
fa_ratio_t fa_ratio_recip(fa_ratio_t);

/** Return the absolute value of the given rational number.
*/
fa_ratio_t fa_ratio_absolute(fa_ratio_t);

/** Normalize the given rational number.
*/
fa_ratio_t fa_ratio_normalize(fa_ratio_t);

/** Convert the given rational number to mixed form.

    For example \f$11/3\f$ becomes \f$3+2/3\f$.
*/
void fa_ratio_to_mixed(fa_ratio_t,
                       fa_ratio_numerator_t *,
                       fa_ratio_t *);

/** @}
    @}
    */

#endif // _FA_RATIO

