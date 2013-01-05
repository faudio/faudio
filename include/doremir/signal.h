
#ifndef _DOREMIR_SIGNAL
#define _DOREMIR_SIGNAL

#include <doremir.h>
#include <doremir/pair.h>
#include <doremir/type.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirSignal Signal
    @{
    */

typedef struct _doremir_signal_t * doremir_signal_t;
doremir_type_t doremir_signal_type_of(doremir_signal_t);
doremir_signal_t doremir_signal_apply_unary(doremir_processor_any_t,
                                            doremir_signal_t);
doremir_signal_t doremir_signal_apply_binary(doremir_processor_any_t,
                                             doremir_signal_t,
                                             doremir_signal_t);
doremir_signal_t doremir_signal_apply_ternary(doremir_processor_any_t,
                                              doremir_signal_t,
                                              doremir_signal_t,
                                              doremir_signal_t);
doremir_signal_t doremir_signal_const(doremir_ptr_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_delay(size_t, doremir_signal_t);
doremir_pair_t doremir_signal_split(doremir_signal_t);
doremir_signal_t doremir_signal_cos(doremir_signal_t);
doremir_signal_t doremir_signal_sin(doremir_signal_t);
doremir_signal_t doremir_signal_tan(doremir_signal_t);
doremir_signal_t doremir_signal_acos(doremir_signal_t);
doremir_signal_t doremir_signal_asin(doremir_signal_t);
doremir_signal_t doremir_signal_atan(doremir_signal_t);
doremir_signal_t doremir_signal_add(doremir_signal_t);
doremir_signal_t doremir_signal_subtract(doremir_signal_t,
                                         doremir_signal_t);
doremir_signal_t doremir_signal_multiply(doremir_signal_t,
                                         doremir_signal_t);
doremir_signal_t doremir_signal_divide(doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_modulo(doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_absolute(doremir_signal_t,
                                         doremir_signal_t);
doremir_signal_t doremir_signal_and(doremir_signal_t,
                                    doremir_signal_t);
doremir_signal_t doremir_signal_or(doremir_signal_t,
                                   doremir_signal_t);
doremir_signal_t doremir_signal_not(doremir_signal_t,
                                    doremir_signal_t);
doremir_signal_t doremir_signal_bit_and(doremir_signal_t,
                                        doremir_signal_t);
doremir_signal_t doremir_signal_bit_or(doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_bit_not(doremir_signal_t,
                                        doremir_signal_t);
doremir_signal_t doremir_signal_bit_xor(doremir_signal_t,
                                        doremir_signal_t);

/** @}
    @}
    */

#endif // _DOREMIR_SIGNAL

