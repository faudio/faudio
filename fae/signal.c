

#include <fae/signal.h>
#include <fae/util.h>

typedef fae_signal_t signal_t;

/*
    Signals are built from the following primitives:

        constant            : a                  -> ~f32
        identity            :                       a ~> a
        lift                : (a -> b)           -> a ~> b
        lift2               : (a -> b -> c)      -> a ~> b ~> c
        lift3               : (a -> b -> c -> d) -> a ~> b ~> c ~> d
        time                :                       ~Time
        delay               : Time               -> a ~> a
        read                : Int                -> ~a
        write               : Int                -> a ~> a
        
        constant            : a                  -> S a
        identity            :                       S a -> S a
        lift                : (a -> b)           -> S a -> S b
        lift2               : (a -> b -> c)      -> S a -> S b -> S c
        lift3               : (a -> b -> c -> d) -> S a -> S b -> S c -> S d
        time                :                       S Time
        delay               : Time               -> S a -> S a
        read                : Int                -> S a
        write               : Int                -> S a

        apply               :  (S a -> S b) -> S a -> S b

        const x     returns a signal of arity 0
        identity    returns a signal of arity 1
        lift  f     returns a signal of arity 1
        lift2 f     returns a signal of arity 2
        lift3 f     returns a signal of arity 3
        time        returns a signal of arity 0
        delay t     returns a signal of arity 1
        read n      returns a signal of arity 0
        write n     returns a signal of arity 1

        apply f x   where f has arity n returns a signal of arity (n-1).
        
    Note that apply is not (<*>), of Applicative, it is simply primitive application of
    two signals (as this implementation includes partial application support).

 */
struct _fae_signal_t {
    
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
        struct {
            ptr_t           value;
        }                   constant;
        struct {
            uint8_t         arity;
            uint8_t         saturation;
            signal_t        arguments[256];
        }                   identity;
        struct {
            uint8_t         arity;
            uint8_t         saturation;
            signal_t        arguments[256];
            ptr_t           function;
            ptr_t           data;
        }                   lifted;
        struct {
            time_t          time;
        }                   time;
        struct {
            uint8_t         arity;
            uint8_t         saturation;
            signal_t        arguments[256];
            time_t          time;
        }                   delay;
        struct {
            int             address;
        }                   read;
        struct {
            uint8_t         arity;
            uint8_t         saturation;
            signal_t        arguments[256];
            int             address;
        }                   write;

    }                       fields;

};

// --------------------------------------------------------------------------------

inline static signal_t new_signal(int tag)
{
    fae_ptr_t signal_impl(fae_id_t interface);
    assert(false);
}

inline static void delete_signal(signal_t signal)
{
    assert(false);
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

fae_type_t fae_signal_type_of(fae_signal_t signal)
{
    assert(false);
}

fae_signal_t fae_signal_constant(fae_ptr_t value)
{
    signal_t signal = new_signal(constant_signal);
    constant_get(signal, value) = value;
    return signal;
}

fae_signal_t fae_signal_identity()
{
    signal_t signal = new_signal(identity_signal);
    identity_get(signal, arity)      = 1;
    identity_get(signal, saturation) = 0;
    return signal;
}

fae_signal_t fae_signal_apply(fae_signal_t signal1, fae_signal_t signal2)
{
    assert(false);
}

fae_signal_t fae_signal_lift(fae_unary_t function, fae_ptr_t data)
{
    signal_t signal = new_signal(lifted_signal);
    lifted_get(signal, function)     = function;
    lifted_get(signal, data)         = data;
    lifted_get(signal, arity)        = 1;
    lifted_get(signal, saturation)   = 0;
    return signal;
}

fae_signal_t fae_signal_lift2(fae_binary_t function, fae_ptr_t data)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_lift3(fae_ternary_t function, fae_ptr_t data)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_time()
{
    return new_signal(time_signal);
}

fae_signal_t fae_signal_delay(fae_time_t time, fae_signal_t signal)
{
    assert(false && "Not implemented");
}

fae_signal_t fae_signal_fix(fae_signal_t (*function)(fae_ptr_t, fae_signal_t), fae_ptr_t data)
{
    assert(false && "Not implemented");
}

// TODO destroy





/*
    This is were the magic happens. The compute(context, signal) computes
    the value of the signal in a context.
 */

typedef struct {                                           
    int64_t count;           // invocation count (monotonically increasing with time)
    time_t  time;            // current time (if applicable)
    double  rate;            // samples per second (if applicable)

    ptr_t   buses[0xffff];   // buses
} context_t;
 
static ptr_t compute(context_t context, fae_signal_t signal);
// static void before(context_t context, fae_signal_t signal);
// static void after(context_t context, fae_signal_t signal);


// void before(context_t context, fae_signal_t signal) {}
// void after(context_t context, fae_signal_t signal) {}

ptr_t compute_constant(context_t context, signal_t signal) {
    return constant_get(signal, value);
}   

ptr_t compute_identity(context_t context, signal_t signal) {
    // TODO assert saturated
    signal_t arg1 = identity_get(signal, arguments)[0];
    return compute(context, arg1);
}

ptr_t compute_lifted(context_t context, signal_t signal) {
    // TODO assert saturated
    switch (lifted_get(signal, arity)) {
        case 1: {
            signal_t arg1 = lifted_get(signal, arguments)[0];
            unary_t  func = lifted_get(signal, function);
            ptr_t    data = lifted_get(signal, data);
            return func(data, 
                compute(context, arg1));
        }
        // case 2:
        // case 3:
        default: {
            assert(false && "Strange arity");
        }
    }
}
ptr_t compute_time(context_t context, signal_t signal) {
    return time_get(signal, time);
}
ptr_t compute_delay(context_t context, signal_t signal) {
    assert(false && "Not implemented");
}
ptr_t compute_read(context_t context, signal_t signal) {
    assert(false && "Not implemented");
}
ptr_t compute_write(context_t context, signal_t signal) {
    assert(false && "Not implemented");
}

ptr_t compute(context_t context, fae_signal_t signal) {
    match(signal->tag) {
        against (constant_signal)   compute_constant(context, signal);
        against (identity_signal)   compute_identity(context, signal);
        against (lifted_signal)     compute_lifted(context, signal);
        against (time_signal)       compute_time(context, signal);
        against (delay_signal)      compute_delay(context, signal);
        against (read_signal)       compute_read(context, signal);
        against (write_signal)      compute_write(context, signal);
        no_default();
    }
}
























// TODO optimized version for non-ptr types

static ptr_t _add(ptr_t c, ptr_t x, ptr_t y) { return fae_add(x, y); }
fae_signal_t fae_signal_add()
{
    return fae_signal_lift2(_add, NULL);
}

static ptr_t _subtract(ptr_t c, ptr_t x, ptr_t y) { return fae_subtract(x, y); }
fae_signal_t fae_signal_subtract()
{
    return fae_signal_lift2(_subtract, NULL);
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

