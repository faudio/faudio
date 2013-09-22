
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/signal.h>
#include <fa/util.h>

typedef fa_signal_t signal_t;

struct _fa_signal_t {

    impl_t          impl;       //  Interface dispatcher

    enum {
        constant_signal,
        identity_signal,
        lifted_signal,
        time_signal,
        delay_signal,
        read_signal,
        write_signal
    }                       tag;

    union {


    }                       fields;

};

// --------------------------------------------------------------------------------

inline static signal_t new_signal(int tag)
{
    fa_ptr_t signal_impl(fa_id_t interface);

    signal_t s = fa_new(signal);
    s->impl = &signal_impl;
    s->tag  = tag;
    return s;
}

inline static void delete_signal(signal_t signal)
{
    fa_delete(signal);
}

#define is_constant(v)    (v->tag == constant_signal)
#define is_identity(v)    (v->tag == identity_signal)
#define is_lifted(v)      (v->tag == lifted_signal)
#define is_time(v)        (v->tag == time_signal)
#define is_delay(v)       (v->tag == delay_signal)
#define is_read(v)        (v->tag == read_signal)
#define is_write(v)       (v->tag == write_signal)

#define constant_get(v,f) v->fields.constant.f
#define identity_get(v,f) v->fields.identity.f
#define lifted_get(v,f)   v->fields.lifted.f
#define time_get(v,f)     v->fields.time.f
#define delay_get(v,f)    v->fields.delay.f
#define read_get(v,f)     v->fields.read.f
#define write_get(v,f)    v->fields.write.f

// --------------------------------------------------------------------------------

fa_type_t fa_signal_type_of(fa_signal_t signal)
{
    assert(false);
}


signal_t copy_constant(signal_t signal)
{
    assert(false && "Not implemented");
}

signal_t copy_identity(signal_t signal)
{
    assert(false && "Not implemented");
}

signal_t copy_lifted(signal_t signal)
{
    assert(false && "Not implemented");
}

signal_t copy_time(signal_t signal)
{
    assert(false && "Not implemented");
}

signal_t copy_delay(signal_t signal)
{
    assert(false && "Not implemented");
}

signal_t copy_read(signal_t signal)
{
    assert(false && "Not implemented");
}

signal_t copy_write(signal_t signal)
{
    assert(false && "Not implemented");
}

fa_signal_t fa_signal_copy(fa_signal_t signal)
{
    match(signal->tag) {
        against(constant_signal)   copy_constant(signal);
        against(identity_signal)   copy_identity(signal);
        against(lifted_signal)     copy_lifted(signal);
        against(time_signal)       copy_time(signal);
        against(delay_signal)      copy_delay(signal);
        against(read_signal)       copy_read(signal);
        against(write_signal)      copy_write(signal);
        no_default();
    }
}

























// TODO optimized version for non-ptr types

fa_signal_t fa_signal_add()
{
    assert(false);
}

fa_signal_t fa_signal_subtract()
{
    assert(false);
}

fa_signal_t fa_signal_multiply()
{
    assert(false);
}

fa_signal_t fa_signal_power()
{
    assert(false);
}

fa_signal_t fa_signal_divide()
{
    assert(false);
}

fa_signal_t fa_signal_modulo()
{
    assert(false);
}

fa_signal_t fa_signal_absolute()
{
    assert(false);
}

fa_signal_t fa_signal_not()
{
    assert(false);
}

fa_signal_t fa_signal_and()
{
    assert(false);
}

fa_signal_t fa_signal_or()
{
    assert(false);
}

fa_signal_t fa_signal_xor()
{
    assert(false);
}

fa_signal_t fa_signal_bit_not()
{
    assert(false);
}

fa_signal_t fa_signal_bit_and()
{
    assert(false);
}

fa_signal_t fa_signal_bit_or()
{
    assert(false);
}

fa_signal_t fa_signal_bit_xor()
{
    assert(false);
}

fa_signal_t fa_signal_shift_left()
{
    assert(false);
}

fa_signal_t fa_signal_shift_right()
{
    assert(false);
}

fa_signal_t fa_signal_equal()
{
    assert(false);
}

fa_signal_t fa_signal_less_than()
{
    assert(false);
}

fa_signal_t fa_signal_greater_than()
{
    assert(false);
}

fa_signal_t fa_signal_less_than_equal()
{
    assert(false);
}

fa_signal_t fa_signal_greater_than_equal()
{
    assert(false);
}

fa_signal_t fa_signal_acos()
{
    assert(false);
}

fa_signal_t fa_signal_asin()
{
    assert(false);
}

fa_signal_t fa_signal_atan()
{
    assert(false);
}

fa_signal_t fa_signal_cos()
{
    assert(false);
}

fa_signal_t fa_signal_sin()
{
    assert(false);
}

fa_signal_t fa_signal_tan()
{
    assert(false);
}

fa_signal_t fa_signal_exp()
{
    assert(false);
}

fa_signal_t fa_signal_log()
{
    assert(false);
}

fa_signal_t fa_signal_log10()
{
    assert(false);
}

fa_signal_t fa_signal_pow()
{
    assert(false);
}

fa_signal_t fa_signal_sqrt()
{
    assert(false);
}

fa_signal_t fa_signal_abs()
{
    assert(false);
}

fa_signal_t fa_signal_min()
{
    assert(false);
}

fa_signal_t fa_signal_max()
{
    assert(false);
}

fa_signal_t fa_signal_fmod()
{
    assert(false);
}

fa_signal_t fa_signal_remainder()
{
    assert(false);
}

fa_signal_t fa_signal_floor()
{
    assert(false);
}

fa_signal_t fa_signal_ceil()
{
    assert(false);
}

fa_signal_t fa_signal_rint()
{
    assert(false);
}



// --------------------------------------------------------------------------------

fa_ptr_t signal_impl(fa_id_t interface)
{

    switch (interface) {
    default:
        return NULL;
    }
}

