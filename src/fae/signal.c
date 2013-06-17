

#include <fae/signal.h>
#include <fae/util.h>


fae_type_t fae_signal_type_of(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_lift(fae_unary_t func,
                                     fae_ptr_t data,
                                     fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_lift2(fae_binary_t func,
                                      fae_ptr_t data,
                                      fae_signal_t signal1,
                                      fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_lift3(fae_ternary_t func,
                                      fae_ptr_t data,
                                      fae_signal_t signal1,
                                      fae_signal_t signal2,
                                      fae_signal_t signal3)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_apply(fae_processor_t proc,
                                      fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_apply2(fae_processor_t proc,
                                       fae_signal_t signal1,
                                       fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_apply3(fae_processor_t proc,
                                       fae_signal_t signal1,
                                       fae_signal_t signal2,
                                       fae_signal_t signal3)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_constant(fae_ptr_t value)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_value(fae_event_t event)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_delay(fae_time_t time, fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_add(fae_signal_t signal,
                                    fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_subtract(fae_signal_t signal,
                                         fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_multiply(fae_signal_t signal,
                                         fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_power(fae_signal_t signal,
                                      fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_divide(fae_signal_t signal,
                                       fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_modulo(fae_signal_t signal,
                                       fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_absolute(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_not(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_and(fae_signal_t signal,
                                    fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_or(fae_signal_t signal,
                                   fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_xor(fae_signal_t signal,
                                    fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_bit_not(fae_signal_t signal,
                                        fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_bit_and(fae_signal_t signal,
                                        fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_bit_or(fae_signal_t signal,
                                       fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_bit_xor(fae_signal_t signal,
                                        fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_shift_left(fae_signal_t signal,
                                           fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_shift_right(fae_signal_t signal,
                                            fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_equal(fae_signal_t signal,
                                      fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_less_than(fae_signal_t signal,
                                          fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_greater_than(fae_signal_t signal,
                                             fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_less_than_equal(fae_signal_t signal,
                                                fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_greater_than_equal(fae_signal_t signal,
                                                   fae_signal_t signal2)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_acos(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_asin(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_atan(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_cos(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_sin(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_tan(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_exp(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_log(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_log10(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_pow(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_sqrt(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_abs(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_min(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_max(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_fmod(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_remainder(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_floor(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_ceil(fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_rint(fae_signal_t signal)
{
    assert(false && "Not implemented");
}


