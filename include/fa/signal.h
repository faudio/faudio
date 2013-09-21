
#ifndef _FA_SIGNAL
#define _FA_SIGNAL

#include <fa.h>
#include <fa/pair.h>
#include <fa/type.h>
#include <fa/time.h>

/** @addtogroup FaSignal

    @addtogroup FaSignal
    
    Provides signals.
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaSignal Signal
    @{
    */


typedef struct _fa_signal_t * fa_signal_t;

/** @typedef fa_signal_unary_signal_t
    Like fa_unary_t, but speficied on signals.
    
*/
typedef fa_signal_t (* fa_signal_unary_signal_t)(fa_ptr_t,
                                                 fa_signal_t);

/** @typedef fa_signal_unary_double_t
    Like fa_unary_t, but speficied on doubles.
    
*/
typedef double (* fa_signal_unary_double_t)(fa_ptr_t, double);

/** @typedef fa_signal_binary_double_t
    Like fa_binary_t, but speficied on doubles.
    
*/
typedef double (* fa_signal_binary_double_t)(fa_ptr_t,
                                             double,
                                             double);


fa_signal_t fa_signal_time();


fa_signal_t fa_signal_random();


fa_signal_t fa_signal_constant(double);


fa_signal_t fa_signal_lift(fa_signal_unary_double_t (fa_string_t),
                           fa_ptr_t,
                           fa_signal_t);


fa_signal_t fa_signal_lift2(fa_signal_binary_double_t (fa_string_t),
                            fa_ptr_t,
                            fa_signal_t,
                            fa_signal_t);


fa_signal_t fa_signal_loop(fa_signal_unary_signal_t, fa_ptr_t);


fa_signal_t fa_signal_delay(int, fa_signal_t);


fa_signal_t fa_signal_input(int);


fa_signal_t fa_signal_output(int, int, fa_signal_t);


fa_signal_t fa_signal_add(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_subtract(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_multiply(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_power(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_divide(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_modulo(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_absolute(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_not();


fa_signal_t fa_signal_and(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_or(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_xor(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_not(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_and(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_or(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_bit_xor(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_shift_left(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_shift_right(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_equal(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_less_than(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_greater_than(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_less_than_equal(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_greater_than_equal(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_acos(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_asin(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_atan(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_cos(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_sin(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_tan(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_exp(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_log(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_log10(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_pow(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_sqrt(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_abs(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_min(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_max(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_fmod(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_remainder(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_floor(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_ceil(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_rint(fa_signal_t, fa_signal_t);


bool fa_signal_is_variable(fa_signal_t);


bool fa_signal_is_constant(fa_signal_t);


bool fa_signal_are_constant(fa_list_t);


int fa_signal_signal_node_count(fa_signal_t);


int fa_signal_required_inputs(fa_signal_t);


int fa_signal_required_buses(fa_signal_t);


int fa_signal_required_delay(fa_signal_t);


fa_signal_t fa_signal_simplify(fa_signal_t);


fa_signal_t fa_signal_latter(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_former(fa_signal_t, fa_signal_t);


fa_signal_t fa_signal_impulse();


fa_signal_t fa_signal_line(double);


fa_signal_t fa_signal_low_pass(fa_signal_t,
                               fa_signal_t,
                               fa_signal_t,
                               fa_signal_t,
                               fa_signal_t);


fa_signal_t fa_signal_biquad(fa_signal_t,
                             fa_signal_t,
                             fa_signal_t,
                             fa_signal_t,
                             fa_signal_t,
                             fa_signal_t);

/** @}
    @}
    */

#endif // _FA_SIGNAL

