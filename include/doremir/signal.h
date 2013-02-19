
#ifndef _DOREMIR_SIGNAL
#define _DOREMIR_SIGNAL

#include <doremir.h>
#include <doremir/pair.h>
#include <doremir/type.h>
#include <doremir/time.h>
#include <doremir/event.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirSignal Signal
    @{
    */

typedef struct _doremir_signal_t * doremir_signal_t;
doremir_type_t doremir_signal_type_of(doremir_signal_t);
doremir_signal_t doremir_signal_lift(doremir_unary_t,
                                     doremir_ptr_t,
                                     doremir_signal_t);
doremir_signal_t doremir_signal_lift2(doremir_binary_t,
                                      doremir_ptr_t,
                                      doremir_signal_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_lift3(doremir_ternary_t,
                                      doremir_ptr_t,
                                      doremir_signal_t,
                                      doremir_signal_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_apply(doremir_processor_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_apply2(doremir_processor_t,
                                       doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_apply3(doremir_processor_t,
                                       doremir_signal_t,
                                       doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_constant(doremir_ptr_t);
doremir_signal_t doremir_signal_value(doremir_event_t);
doremir_signal_t doremir_signal_delay(doremir_time_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_add(doremir_signal_t,
                                    doremir_signal_t);
doremir_signal_t doremir_signal_subtract(doremir_signal_t,
                                         doremir_signal_t);
doremir_signal_t doremir_signal_multiply(doremir_signal_t,
                                         doremir_signal_t);
doremir_signal_t doremir_signal_power(doremir_signal_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_divide(doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_modulo(doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_absolute(doremir_signal_t);
doremir_signal_t doremir_signal_not(doremir_signal_t);
doremir_signal_t doremir_signal_and(doremir_signal_t,
                                    doremir_signal_t);
doremir_signal_t doremir_signal_or(doremir_signal_t,
                                   doremir_signal_t);
doremir_signal_t doremir_signal_xor(doremir_signal_t,
                                    doremir_signal_t);
doremir_signal_t doremir_signal_bit_not(doremir_signal_t,
                                        doremir_signal_t);
doremir_signal_t doremir_signal_bit_and(doremir_signal_t,
                                        doremir_signal_t);
doremir_signal_t doremir_signal_bit_or(doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_bit_xor(doremir_signal_t,
                                        doremir_signal_t);
doremir_signal_t doremir_signal_shift_left(doremir_signal_t,
                                           doremir_signal_t);
doremir_signal_t doremir_signal_shift_right(doremir_signal_t,
                                            doremir_signal_t);
doremir_signal_t doremir_signal_equal(doremir_signal_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_less_than(doremir_signal_t,
                                          doremir_signal_t);
doremir_signal_t doremir_signal_greater_than(doremir_signal_t,
                                             doremir_signal_t);
doremir_signal_t doremir_signal_less_than_equal(doremir_signal_t,
                                                doremir_signal_t);
doremir_signal_t doremir_signal_greater_than_equal(doremir_signal_t,
                                                   doremir_signal_t);
doremir_signal_t doremir_signal_acos(doremir_signal_t);
doremir_signal_t doremir_signal_asin(doremir_signal_t);
doremir_signal_t doremir_signal_atan(doremir_signal_t);
doremir_signal_t doremir_signal_cos(doremir_signal_t);
doremir_signal_t doremir_signal_sin(doremir_signal_t);
doremir_signal_t doremir_signal_tan(doremir_signal_t);
doremir_signal_t doremir_signal_exp(doremir_signal_t);
doremir_signal_t doremir_signal_log(doremir_signal_t);
doremir_signal_t doremir_signal_log10(doremir_signal_t);
doremir_signal_t doremir_signal_pow(doremir_signal_t);
doremir_signal_t doremir_signal_sqrt(doremir_signal_t);
doremir_signal_t doremir_signal_abs(doremir_signal_t);
doremir_signal_t doremir_signal_min(doremir_signal_t);
doremir_signal_t doremir_signal_max(doremir_signal_t);
doremir_signal_t doremir_signal_fmod(doremir_signal_t);
doremir_signal_t doremir_signal_remainder(doremir_signal_t);
doremir_signal_t doremir_signal_floor(doremir_signal_t);
doremir_signal_t doremir_signal_ceil(doremir_signal_t);
doremir_signal_t doremir_signal_rint(doremir_signal_t);

/** @}
    @}
    */

#endif // _DOREMIR_SIGNAL

