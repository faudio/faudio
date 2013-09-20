
#ifndef _FAE_SIGNAL
#define _FAE_SIGNAL

#include <fae.h>
#include <fae/pair.h>
#include <fae/type.h>
#include <fae/time.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeSignal Signal
    @{
    */

typedef struct _fae_signal_t * fae_signal_t;
typedef fae_signal_t (* fae_signal_unary_signal_t)(fae_ptr_t,
                                                   fae_signal_t);
typedef double (* fae_signal_unary_double_t)(fae_ptr_t, double);
typedef double (* fae_signal_binary_double_t)(fae_ptr_t,
                                              double,
                                              double);
fae_signal_t fae_signal_time();
fae_signal_t fae_signal_random();
fae_signal_t fae_signal_constant(double);
fae_signal_t fae_signal_lift(fae_signal_unary_double_t (fae_string_t),
                             fae_ptr_t,
                             fae_signal_t);
fae_signal_t fae_signal_lift2(fae_signal_binary_double_t (fae_string_t),
                              fae_ptr_t,
                              fae_signal_t,
                              fae_signal_t);
fae_signal_t fae_signal_loop(fae_signal_unary_signal_t, fae_ptr_t);
fae_signal_t fae_signal_delay(int, fae_signal_t);
fae_signal_t fae_signal_input(int);
fae_signal_t fae_signal_output(int, int, fae_signal_t);
fae_signal_t fae_signal_add(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_subtract(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_multiply(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_power(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_divide(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_modulo(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_absolute(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_not();
fae_signal_t fae_signal_and(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_or(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_xor(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_bit_not(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_bit_and(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_bit_or(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_bit_xor(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_shift_left(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_shift_right(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_equal(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_less_than(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_greater_than(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_less_than_equal(fae_signal_t,
                                        fae_signal_t);
fae_signal_t fae_signal_greater_than_equal(fae_signal_t,
                                           fae_signal_t);
fae_signal_t fae_signal_acos(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_asin(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_atan(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_cos(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_sin(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_tan(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_exp(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_log(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_log10(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_pow(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_sqrt(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_abs(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_min(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_max(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_fmod(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_remainder(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_floor(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_ceil(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_rint(fae_signal_t, fae_signal_t);
bool fae_signal_is_variable(fae_signal_t);
bool fae_signal_is_constant(fae_signal_t);
bool fae_signal_are_constant(fae_list_t);
int fae_signal_signal_node_count(fae_signal_t);
int fae_signal_required_inputs(fae_signal_t);
int fae_signal_required_buses(fae_signal_t);
int fae_signal_required_delay(fae_signal_t);
fae_signal_t fae_signal_simplify(fae_signal_t);
fae_signal_t fae_signal_latter(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_former(fae_signal_t, fae_signal_t);
fae_signal_t fae_signal_impulse();
fae_signal_t fae_signal_line(double);
fae_signal_t fae_signal_low_pass(fae_signal_t,
                                 fae_signal_t,
                                 fae_signal_t,
                                 fae_signal_t,
                                 fae_signal_t);
fae_signal_t fae_signal_biquad(fae_signal_t,
                               fae_signal_t,
                               fae_signal_t,
                               fae_signal_t,
                               fae_signal_t,
                               fae_signal_t);

/** @}
    @}
    */

#endif // _FAE_SIGNAL

