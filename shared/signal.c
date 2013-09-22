
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/signal.h>
#include <fa/buffer.h>
#include <fa/util.h>

typedef fa_signal_t                 signal_t;
typedef fa_signal_unary_signal_t    s2s_t;
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
            d2d_t f;
            ptr_t fd;
            signal_t a;
        } lift;

        struct {
            string_t name;
            dd2d_t f;
            ptr_t fd;
            signal_t a;
            signal_t b;
        } lift2;

        struct {
            s2s_t f;
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

#define is_time(v)          (v->tag == time_signal)
#define is_random(v)        (v->tag == random_signal)
#define is_constant(v)      (v->tag == constant_signal)
#define is_lift(v)          (v->tag == lift_signal)
#define is_lift2(v)         (v->tag == lift2_signal)
#define is_input(v)         (v->tag == input_signal)
#define is_output(v)        (v->tag == output_signal)

#define time_get(v,f)       v->fields.time.f
#define random_get(v,f)     v->fields.random.f
#define constant_get(v,f)   v->fields.constant.f
#define lift_get(v,f)       v->fields.lift.f
#define lift2_get(v,f)      v->fields.lift2.f
#define input_get(v,f)      v->fields.input.f
#define output_get(v,f)     v->fields.output.f

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

fa_signal_t fa_signal_constant(double x)
{
    signal_t signal = new_signal(constant_signal);
    signal->fields.constant.value = x;
    return signal;
}


fa_signal_t fa_signal_lift(fa_string_t n,
                           fa_signal_unary_double_t f,
                           fa_ptr_t fd,
                           fa_signal_t a)
{
    signal_t signal = new_signal(lift_signal);
    lift_get(signal,name) = n;
    lift_get(signal,f)    = f;
    lift_get(signal,fd)   = fd;
    lift_get(signal,a)    = a;
    return signal;
}


fa_signal_t fa_signal_lift2(fa_string_t n,
                            fa_signal_binary_double_t f,
                            fa_ptr_t fd,
                            fa_signal_t a,
                            fa_signal_t b)
{
    signal_t signal = new_signal(lift2_signal);
    signal->fields.lift2.name = n;
    signal->fields.lift2.f  = f;
    signal->fields.lift2.fd = fd;
    signal->fields.lift2.a  = a;
    signal->fields.lift2.b  = b;
    return signal;
}

/*

fa_signal_t fa_signal_loop(fa_signal_unary_signal_t, fa_ptr_t)
{
}


fa_signal_t fa_signal_delay(int, fa_signal_t)
{
}


fa_signal_t fa_signal_input(int)
{
}


fa_signal_t fa_signal_output(int, int, fa_signal_t)
{
}
*/

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







bool fa_signal_is_variable(fa_signal_t a)
{
    if (a->tag == constant_signal) {
        return false;
    }

    if (a->tag == random_signal) {
        return true;
    }

    if (a->tag == time_signal) {
        return true;
    }

    if (a->tag == input_signal) {
        return true;
    }


    if (a->tag == lift_signal) {
        return fa_signal_is_variable(a->fields.lift.a);
    }

    if (a->tag == lift2_signal) {
        return fa_signal_is_variable(a->fields.lift2.a) && fa_signal_is_variable(a->fields.lift2.b);
    }

    if (a->tag == output_signal) {
        return fa_signal_is_variable(a->fields.output.a);
    }

    assert(false);
}


bool fa_signal_is_constant(fa_signal_t a)
{
    return !fa_signal_is_variable(a);
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
    double *inputs;
    double *buses;

    int count;
    double rate;

}  _state_t;
typedef _state_t *state_t;


double  kRate       = 44100;
int     kMaxInputs  = 1024;
int     kMaxBuses   = 1024;
int     kMaxDelay   = 44100 * 60 * 5;

state_t new_state()
{
    srand(time(NULL));  // TODO
    state_t state = fa_malloc(sizeof(_state_t));

    state->inputs = fa_malloc(kMaxInputs);
    state->buses  = fa_malloc(kMaxBuses * kMaxDelay);
    memset(state->inputs, 0, kMaxInputs);
    memset(state->buses, 0,  kMaxBuses * kMaxDelay);

    state->count = 0;
    state->rate  = kRate;

    return state;
}
double state_random(state_t state)
{
    return ((double)rand() / (double)RAND_MAX) * 2 - 1;
}
double state_time(state_t state)
{
    return state->count / state->rate;
}
double read_samp(int c, state_t state)
{
    assert(false && "read_samp");
}
double write_samp(int n, int c, double x, state_t state)
{
    assert(false && "write_samp");
}
void inc_state(state_t state)
{
    state->count++;
}

double step(signal_t signal, state_t state)
{
    switch (signal->tag) {
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

// No allocation in loop
// Use ringbuffers for transfer

void fa_signal_run(int n, signal_t a, double *output)
{
    state_t state = new_state();
    // TODO optimize
    // TODO simplify
    // TODO verify

    // pair_t p = pair(i32(1),i32(2));
    for (int i = 0; i < n; ++ i) {
        // p = fa_pair_swap(p);
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

buffer_t fa_signal_run_buffer(int n, signal_t a)
{
    buffer_t b = fa_buffer_create(n * sizeof(double));
    fa_signal_run(n, a, fa_buffer_unsafe_address(b));
    return b;
}

ptr_t fa_signal_run_file(int n, signal_t a, string_t path)
{
    buffer_t b = fa_signal_run_buffer(n, a);
    ptr_t res = fa_buffer_write_audio(path, NULL, b);
    fa_destroy(b);
    return res;
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











double _add(ptr_t _, double x, double y)
{
    return x + y;
}
double _mul(ptr_t _, double x, double y)
{
    return x * y;
}
double _sin(ptr_t _, double x)
{
    return sin(x);
}

fa_signal_t fa_signal_add(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(
               string("(+)"), _add, NULL, a, b);
}


fa_signal_t fa_signal_multiply(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(
               string("(*)"), _mul, NULL, a, b);
}


fa_signal_t fa_signal_sin(fa_signal_t a)
{
    return fa_signal_lift(
               string("sin"), _sin, NULL, a);
}



























// --------------------------------------------------------------------------------

fa_ptr_t signal_impl(fa_id_t interface)
{

    switch (interface) {
    default:
        return NULL;
    }
}

