
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/signal.h>
#include <fa/util.h>

typedef fa_signal_t                 signal_t;
typedef fa_signal_unary_double_t    d2d_t;
typedef fa_signal_binary_double_t   dd2d_t;

struct _fa_signal_t {

    impl_t          impl;       //  Interface dispatcher

    enum {
        time_signal,
        random_signal,
        constant_signal,
        lift_signal,
        lift2_signal,
        loop_signal,
        delay_signal,
        input_signal,
        output_signal
    }                       tag;

    union {
        struct {} time;

        struct {} random;

        struct {
            double value;
        } constant;

        struct {
            string_t name;
            fa_signal_unary_double_t f;
            signal_t a;
        } lift;

        struct {
            string_t name;
            fa_signal_binary_double_t f;
            signal_t a;
            signal_t b;
        } lift2;

        struct {
            fa_signal_unary_signal_t f;
        } loop;

        struct {
            int n;
            signal_t a;
        } delay;

        struct {
            int c;
        } input;

        struct {
            int n;
            int c;
            signal_t a;
        } output;
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

signal_t fa_signal_time()
{
    signal_t signal = new_signal(time_signal);
    return signal;
}

signal_t fa_signal_random()
{
    signal_t signal = new_signal(random_signal);
    return signal;
}

bool fa_signal_is_variable(fa_signal_t a)
{
    assert(false && "Not implemented");
}


bool fa_signal_is_constant(fa_signal_t a)
{
    assert(false && "Not implemented");
}


bool fa_signal_are_constant(fa_list_t a)
{
    assert(false && "Not implemented");
}


int fa_signal_signal_node_count(fa_signal_t a)
{
    assert(false && "Not implemented");
}


int fa_signal_required_inputs(fa_signal_t a)
{
    assert(false && "Not implemented");
}


int fa_signal_required_buses(fa_signal_t a)
{
    assert(false && "Not implemented");
}


int fa_signal_required_delay(fa_signal_t a)
{
    assert(false && "Not implemented");
}


fa_signal_t fa_signal_simplify(fa_signal_t a)
{
    assert(false && "Not implemented");
}


fa_signal_t fa_signal_latter(fa_signal_t a, fa_signal_t b)
{
    assert(false && "Not implemented");
}


fa_signal_t fa_signal_former(fa_signal_t a, fa_signal_t b)
{
    assert(false && "Not implemented");
}


fa_signal_t fa_signal_impulse()
{
    assert(false && "Not implemented");
}


fa_signal_t fa_signal_line(double x)
{
    assert(false && "Not implemented");
}



typedef struct {
    double* inputs;
    double* buses;
    
    int count;
    double rate;
    
}  _state_t;
typedef _state_t *state_t;


double  kRate       = 44100;
int     kMaxInputs  = 1024;
int     kMaxBuses   = 20;
int     kMaxDelay   = 44100*60*5;

state_t new_state()
{                 
    srand (time(NULL)); // TODO
    state_t state = fa_malloc(sizeof(_state_t));

    state->inputs = fa_malloc(kMaxInputs);
    state->buses  = fa_malloc(kMaxBuses*kMaxDelay);
    memset(state->inputs, 0, kMaxInputs);
    memset(state->buses, 0,  kMaxBuses*kMaxDelay);

    state->count = 0;
    state->rate  = kRate;

    return state; 
}
double state_random(state_t state) { 
    return rand();
}
double state_time(state_t state) { 
    return state->count / state->rate; 
}
double read_samp(int c, state_t state) { 
    assert(false && "read_samp");
}
double write_samp(int n, int c, double x, state_t state) { 
    assert(false && "write_samp");
}
void inc_state(state_t state) { 
    state->count++;
}

double step(signal_t signal, state_t state)
{
    switch(signal->tag) {
        case time_signal:
            return state_time(state);
        case random_signal:
            return state_random(state);
        case constant_signal:
            return signal->fields.constant.value;
        case lift_signal: {
            d2d_t    f = signal->fields.lift.f;
            signal_t a = signal->fields.lift.a;
            double xa = step(a, state);
            return f(NULL, xa);
        }
        case lift2_signal: {
            dd2d_t   f = signal->fields.lift2.f;
            signal_t a = signal->fields.lift2.a;
            signal_t b = signal->fields.lift2.b;
            double xa = step(a, state);
            double xb = step(b, state);
            return f(NULL, xa, xb);
        }
        case input_signal: {
            int c = signal->fields.input.c;
            return read_samp(c, state);
        }
        case output_signal: {
            int n = signal->fields.output.n;
            int c = signal->fields.output.c;
            signal_t a = signal->fields.output.a;
            
            double xa = step(a, state);
            write_samp(n, c, xa, state);
            return xa;
        }
        default:
            assert(false && "step: Strange signal");
    }       
    assert(false);
}

void fa_signal_run(int n, signal_t a, double* output)
{
    state_t state = new_state();
    // TODO optimize
    // TODO simplify
    // TODO verify
    
    for (int i = 0; i < n; ++ i) {
        output[i] = step(a, state);
        inc_state(state);
    }
}

void fa_signal_print(int n, signal_t a)
{
    state_t state = new_state();
    // TODO optimize
    // TODO simplify
    // TODO verify
    
    for (int i = 0; i < n; ++ i) {
        double x = step(a, state);
        printf("%3d: %4f\n", i, x);
        inc_state(state);
    }
}


// fa_signal_t fa_signal_low_pass(fa_signal_t,
//                                fa_signal_t,
//                                fa_signal_t,
//                                fa_signal_t,
//                                fa_signal_t)
// {
// }
// 
// 
// fa_signal_t fa_signal_biquad(fa_signal_t,
//                              fa_signal_t,
//                              fa_signal_t,
//                              fa_signal_t,
//                              fa_signal_t,
//                              fa_signal_t)
// {
// }    









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
        against(time_signal)        copy_constant(signal);
        against(random_signal)      copy_identity(signal);
        against(constant_signal)    copy_lifted(signal);
        against(lift_signal)        copy_time(signal);
        against(lift2_signal)       copy_delay(signal);
        against(loop_signal)        copy_read(signal);
        against(delay_signal)       copy_write(signal);
        against(input_signal)       copy_write(signal);
        against(output_signal)      copy_write(signal);
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

