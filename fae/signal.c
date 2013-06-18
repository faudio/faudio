

#include <fae/signal.h>
#include <fae/util.h>

typedef fae_signal_t signal_t;

struct _fae_signal_t {
    impl_t          impl;       //  Interface dispatcher
};

// --------------------------------------------------------------------------------

inline static signal_t new_signal()
{
    fae_ptr_t signal_impl(fae_id_t interface);
}

inline static void delete_signal(signal_t signal)
{
}

// --------------------------------------------------------------------------------

fae_type_t fae_signal_type_of(fae_signal_t signal)
{
    assert(false);
}

fae_signal_t fae_signal_constant(fae_ptr_t value)
{
    assert(false);
}

fae_signal_t fae_signal_identity(fae_signal_t signal)
{
    assert(false);
}

fae_signal_t fae_signal_apply(fae_signal_t signal1, fae_signal_t signal2)
{
    assert(false);
}

fae_signal_t fae_signal_lift(fae_unary_t function, fae_ptr_t data)
{
    assert(false);
}

fae_signal_t fae_signal_lift2(fae_binary_t function, fae_ptr_t data)
{
    assert(false);
}

fae_signal_t fae_signal_lift3(fae_ternary_t function, fae_ptr_t data)
{
    assert(false);
}

fae_signal_t fae_signal_time()
{
    assert(false);
}

fae_signal_t fae_signal_delay(fae_time_t time, fae_signal_t signal)
{
    assert(false);
}

fae_signal_t fae_signal_fix(fae_signal_t (*function)(fae_ptr_t, fae_signal_t), fae_ptr_t data)
{
    assert(false);
}

fae_signal_t fae_signal_add()
{
    assert(false);
}

fae_signal_t fae_signal_subtract()
{
    assert(false);
}

fae_signal_t fae_signal_multiply()
{
    assert(false);
}

fae_signal_t fae_signal_power()
{
    assert(false);
}

fae_signal_t fae_signal_divide()
{
    assert(false);
}

fae_signal_t fae_signal_modulo()
{
    assert(false);
}

fae_signal_t fae_signal_absolute()
{
    assert(false);
}

fae_signal_t fae_signal_not()
{
    assert(false);
}

fae_signal_t fae_signal_and()
{
    assert(false);
}

fae_signal_t fae_signal_or()
{
    assert(false);
}

fae_signal_t fae_signal_xor()
{
    assert(false);
}

fae_signal_t fae_signal_bit_not()
{
    assert(false);
}

fae_signal_t fae_signal_bit_and()
{
    assert(false);
}

fae_signal_t fae_signal_bit_or()
{
    assert(false);
}

fae_signal_t fae_signal_bit_xor()
{
    assert(false);
}

fae_signal_t fae_signal_shift_left()
{
    assert(false);
}

fae_signal_t fae_signal_shift_right()
{
    assert(false);
}

fae_signal_t fae_signal_equal()
{
    assert(false);
}

fae_signal_t fae_signal_less_than()
{
    assert(false);
}

fae_signal_t fae_signal_greater_than()
{
    assert(false);
}

fae_signal_t fae_signal_less_than_equal()
{
    assert(false);
}

fae_signal_t fae_signal_greater_than_equal()
{
    assert(false);
}

fae_signal_t fae_signal_acos()
{
    assert(false);
}

fae_signal_t fae_signal_asin()
{
    assert(false);
}

fae_signal_t fae_signal_atan()
{
    assert(false);
}

fae_signal_t fae_signal_cos()
{
    assert(false);
}

fae_signal_t fae_signal_sin()
{
    assert(false);
}

fae_signal_t fae_signal_tan()
{
    assert(false);
}

fae_signal_t fae_signal_exp()
{
    assert(false);
}

fae_signal_t fae_signal_log()
{
    assert(false);
}

fae_signal_t fae_signal_log10()
{
    assert(false);
}

fae_signal_t fae_signal_pow()
{
    assert(false);
}

fae_signal_t fae_signal_sqrt()
{
    assert(false);
}

fae_signal_t fae_signal_abs()
{
    assert(false);
}

fae_signal_t fae_signal_min()
{
    assert(false);
}

fae_signal_t fae_signal_max()
{
    assert(false);
}

fae_signal_t fae_signal_fmod()
{
    assert(false);
}

fae_signal_t fae_signal_remainder()
{
    assert(false);
}

fae_signal_t fae_signal_floor()
{
    assert(false);
}

fae_signal_t fae_signal_ceil()
{
    assert(false);
}

fae_signal_t fae_signal_rint()
{
    assert(false);
}



// --------------------------------------------------------------------------------

fae_ptr_t signal_impl(fae_id_t interface)
{

    switch (interface) {
    default:
        return NULL;
    }
}

