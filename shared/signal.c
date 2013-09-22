
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

    impl_t                  impl;

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
        struct {}           time;

        struct {}           random;

        struct {
            double          value;
        }                   constant;

        struct {
            string_t        name;
            d2d_t           f;
            ptr_t           fd;
            signal_t        a;
        }                   lift;

        struct {
            string_t        name;
            dd2d_t          f;
            ptr_t           fd;
            signal_t        a;
            signal_t        b;
        }                   lift2;

        struct {
            s2s_t           f;
            ptr_t           fd;
        } loop;

        struct {
            int             n;
            signal_t        a;
        } delay;

        struct {
            int             c;
        } input;

        struct {
            int             n;
            int             c;
            signal_t        a;
        }                   output;
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

#define is_delay(v)         (v->tag == delay_signal)
#define is_loop(v)          (v->tag == loop_signal)
#define is_input(v)         (v->tag == input_signal)
#define is_output(v)        (v->tag == output_signal)

#define time_get(v,f)       v->fields.time.f
#define random_get(v,f)     v->fields.random.f
#define constant_get(v,f)   v->fields.constant.f
#define lift_get(v,f)       v->fields.lift.f
#define lift2_get(v,f)      v->fields.lift2.f
#define loop_get(v,f)       v->fields.loop.f
#define delay_get(v,f)      v->fields.delay.f
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
    constant_get(signal, value) = x;
    return signal;
}


fa_signal_t fa_signal_lift(fa_string_t n,
                           fa_signal_unary_double_t f,
                           fa_ptr_t fd,
                           fa_signal_t a)
{
    signal_t signal = new_signal(lift_signal);
    lift_get(signal, name) = n;
    lift_get(signal, f)    = f;
    lift_get(signal, fd)   = fd;
    lift_get(signal, a)    = a;
    return signal;
}


fa_signal_t fa_signal_lift2(fa_string_t n,
                            fa_signal_binary_double_t f,
                            fa_ptr_t fd,
                            fa_signal_t a,
                            fa_signal_t b)
{
    signal_t signal = new_signal(lift2_signal);
    lift2_get(signal, name) = n;
    lift2_get(signal, f)    = f;
    lift2_get(signal, fd)   = fd;
    lift2_get(signal, a)    = a;
    lift2_get(signal, b)    = b;
    return signal;
}

fa_signal_t fa_signal_loop(fa_signal_unary_signal_t f, fa_ptr_t fd)
{
    signal_t signal = new_signal(lift2_signal);
    loop_get(signal, f)  = f;
    loop_get(signal, fd) = fd;
    return signal;
}


fa_signal_t fa_signal_delay(int n, fa_signal_t a)
{
    signal_t signal = new_signal(delay_signal);
    delay_get(signal, n)  = n;
    delay_get(signal, a)  = a;
    return signal;
}


fa_signal_t fa_signal_input(int c)
{
    signal_t signal = new_signal(input_signal);
    input_get(signal, c)  = c;
    return signal;
}


fa_signal_t fa_signal_output(int n, int c, fa_signal_t a)
{
    signal_t signal = new_signal(output_signal);
    output_get(signal, n)  = n;
    output_get(signal, c)  = c;
    output_get(signal, a)  = a;
    return signal;
}

signal_t copy_constant(signal_t signal2)
{
    signal_t signal = new_signal(constant_signal);
    constant_get(signal, value) = constant_get(signal2, value);
    return signal;
}

signal_t copy_lift(signal_t signal2)
{
    signal_t signal = new_signal(lift_signal);
    lift_get(signal, name) = lift_get(signal2, name);
    lift_get(signal, f)    = lift_get(signal2, f);
    lift_get(signal, fd)   = lift_get(signal2, fd);
    lift_get(signal, a)    = lift_get(signal2, a);
    return signal;
}

signal_t copy_lift2(signal_t signal2)
{
    signal_t signal = new_signal(lift2_signal);
    lift2_get(signal, name) = lift2_get(signal2, name);
    lift2_get(signal, f)    = lift2_get(signal2, f);
    lift2_get(signal, fd)   = lift2_get(signal2, fd);
    lift2_get(signal, a)    = lift2_get(signal2, a);
    lift2_get(signal, b)    = lift2_get(signal2, b);
    return signal;
}

signal_t copy_loop(signal_t signal2)
{
    signal_t signal = new_signal(lift2_signal);
    loop_get(signal, f)  = loop_get(signal2, f);
    loop_get(signal, fd) = loop_get(signal2, fd);
    return signal;
}

signal_t copy_delay(signal_t signal2)
{
    signal_t signal = new_signal(delay_signal);
    delay_get(signal, n)  = delay_get(signal2, n);
    delay_get(signal, a)  = delay_get(signal2, a);
    return signal;
}

signal_t copy_input(signal_t signal2)
{
    signal_t signal = new_signal(input_signal);
    input_get(signal, c)  = input_get(signal2, c);
    return signal;
}

signal_t copy_output(signal_t signal2)
{
    signal_t signal = new_signal(output_signal);
    output_get(signal, n)  = output_get(signal2, n);
    output_get(signal, c)  = output_get(signal2, c);
    output_get(signal, a)  = output_get(signal2, a);
    return signal;
}

fa_signal_t fa_signal_copy(fa_signal_t signal)
{
    match(signal->tag) {
        against(time_signal)        fa_signal_time();
        against(random_signal)      fa_signal_random();
        against(constant_signal)    copy_constant(signal);
        against(lift_signal)        copy_lift(signal);
        against(lift2_signal)       copy_lift2(signal);
        against(loop_signal)        copy_loop(signal);
        against(delay_signal)       copy_delay(signal);
        against(input_signal)       copy_input(signal);
        against(output_signal)      copy_output(signal);
        no_default();
    }
}

void fa_signal_destroy(fa_signal_t signal)
{
    // TODO
    delete_signal(signal);
}





bool fa_signal_is_variable(fa_signal_t a)
{
    assert(false && "Not implemented");
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


fa_pair_t   fa_signal_to_tree(fa_signal_t signal)
{
    switch (signal->tag) {
    case time_signal:
        return pair(string("time"), empty());

    case random_signal:
        return pair(string("random"), empty());

    case constant_signal:
        return pair(fa_string_to_string(f64(constant_get(signal, value))), empty());

    case lift_signal:
        return pair(lift_get(signal, name), list(
                        fa_signal_to_tree(lift_get(signal, a))
                    ));

    case lift2_signal:
        return pair(lift_get(signal, name), list(
                        fa_signal_to_tree(lift2_get(signal, a)),
                        fa_signal_to_tree(lift2_get(signal, b))
                    ));

    case input_signal:
        return pair(string("input "), empty());

    case output_signal:
        return pair(string("output "), list(
                        fa_signal_to_tree(lift_get(signal, a))
                    ));

    default:
        assert(false);
    }
}


string_t draw_tree(string_t indent, string_t str, pair_t p, bool is_last)
{
    str = string_append(str, indent);

    if (is_last) {
        str         = string_dappend(str, string("\\-"));
        indent      = string_append(indent, string("  "));
    } else {
        str         = string_dappend(str, string("|-"));
        indent      = string_append(indent, string("| "));
    }

    str = string_dappend(str, fa_string_to_string(fa_pair_first(p)));
    str = string_dappend(str, string("\n"));

    fa_for_each_last(x, fa_pair_second(p), last) {
        str = draw_tree(indent, str, x, last);
    }
    return str;
}

string_t fa_signal_draw_tree(fa_pair_t p)
{
    return draw_tree(string(""), string(""), p, true);
}




/*
    runPart (Part (o,d)) = (o, Part (o+d,d))
    splitPart (Part (o,d)) = (Part (o,d*2), Part (d,d*2))
    nextP = fst . runPart
    skipP = snd . runPart
    runPartAll g = let
        (x,g2) = runPart g
        in x : runPartAll g2
*/

struct part {
    int o, d;
};
void init_part(struct part *p)
{
    p->o = 0;
    p->d = 1;
}
void run_part(struct part *p, int *r, struct part *p2)
{
    r = 0;
    p2->o = p->o + p->d;
    p2->d = p->d;
}
void split_part(struct part *p, struct part *p2, struct part *p3)
{
    p2->o = p->o;
    p2->o = p->d * 2;
    p3->o = p->d;
    p3->o = p->d * 2;
}



fa_signal_t simp(struct part *p, fa_signal_t signal2)
{
    struct part pa;
    struct part pb;

    switch (signal2->tag) {
    case loop_signal:
        return fa_copy(signal2); // TODO

    case delay_signal:
        return fa_copy(signal2); // TODO

    case lift_signal: {
        string_t name   = lift_get(signal2, name);
        d2d_t f         = lift_get(signal2, f);
        ptr_t fd        = lift_get(signal2, fd);
        signal_t a      = simp(p, lift_get(signal2, a));
        return fa_signal_lift(name, f, fd, a);
    }

    case lift2_signal:       {
        split_part(p, &pa, &pb);

        return fa_signal_lift2(
                   lift2_get(signal2, name),
                   lift2_get(signal2, f),
                   lift2_get(signal2, fd),
                   simp(&pa, lift2_get(signal2, a)), // TODO split the partition
                   simp(&pb, lift2_get(signal2, b))
               );
    }

    case output_signal:
        return fa_signal_output(
                   output_get(signal2, n),
                   output_get(signal2, c),
                   simp(p, output_get(signal2, a))
               );

    default:
        return fa_copy(signal2);
    }
}
fa_signal_t fa_signal_simplify(fa_signal_t signal2)
{
    struct part p;
    init_part(&p);
    return simp(&p, signal2);
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
inline static
double state_random(state_t state)
{
    return ((double)rand() / (double)RAND_MAX) * 2 - 1;
}
inline static
double state_time(state_t state)
{
    return state->count / state->rate;
}
inline static
double read_samp(int c, state_t state)
{
    assert(false && "read_samp");
}
inline static
double write_samp(int n, int c, double x, state_t state)
{
    assert(false && "write_samp");
}
inline static
void inc_state(state_t state)
{
    state->count++;
}


/**
    Step over a sample.
 */
inline static
double step(signal_t signal, state_t state)
{
    switch (signal->tag) {

    case time_signal:
        return state_time(state);

    case random_signal:
        return state_random(state);

    case constant_signal:
        return constant_get(signal, value);

    case lift_signal: {
        d2d_t    f      = lift_get(signal, f);
        signal_t a      = lift_get(signal, a);
        double   xa     = step(a, state);
        return f(NULL, xa);
    }

    case lift2_signal: {
        dd2d_t   f      = lift2_get(signal, f);
        signal_t a      = lift2_get(signal, a);
        signal_t b      = lift2_get(signal, b);
        double   xa     = step(a, state);
        double   xb     = step(b, state);
        return f(NULL, xa, xb);
    }

    case input_signal: {
        int     c       = input_get(signal, c);
        return read_samp(c, state);
    }

    case output_signal: {
        int     n       = output_get(signal, n);
        int     c       = output_get(signal, c);
        signal_t a      = output_get(signal, a);

        double  xa      = step(a, state);
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



inline static double _former(ptr_t _, double x, double y)
{
    return x;
}
fa_signal_t fa_signal_former(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(string("former"), _former, NULL, a, b);
}

inline static double _latter(ptr_t _, double x, double y)
{
    return y;
}
fa_signal_t fa_signal_latter(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(string("latter"), _latter, NULL, a, b);
}



inline static double _impulse(ptr_t _, double x)
{
    return (x == 0) ? 1 : 0;
}
fa_signal_t fa_signal_impulse()
{
    return fa_signal_lift(string("mkImp"), _impulse, NULL, fa_signal_time());
}


fa_signal_t fa_signal_line(double x)
{
    double tau = 2 * 3.141592653589793;
    return fa_signal_multiply(fa_signal_time(), fa_signal_constant(x * tau));
}



inline static double _add(ptr_t _, double x, double y)
{
    return x + y;
}
fa_signal_t fa_signal_add(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(string("(+)"), _add, NULL, a, b);
}


inline static double _mul(ptr_t _, double x, double y)
{
    return x * y;
}
fa_signal_t fa_signal_multiply(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(string("(*)"), _mul, NULL, a, b);
}


inline static double _sin(ptr_t _, double x)
{
    return sin(x);
}
fa_signal_t fa_signal_sin(fa_signal_t a)
{
    return fa_signal_lift(string("sin"), _sin, NULL, a);
}

















// --------------------------------------------------------------------------------

ptr_t signal_copy(ptr_t a)
{
    return fa_signal_copy(a);
}

void signal_destroy(ptr_t a)
{
    return fa_signal_destroy(a);
}

fa_ptr_t signal_impl(fa_id_t interface)
{
    static fa_copy_t signal_copy_impl
        = { signal_copy };
    static fa_destroy_t signal_destroy_impl
        = { signal_destroy };

    switch (interface) {
    case fa_copy_i:
        return &signal_copy_impl;

    case fa_destroy_i:
        return &signal_destroy_impl;

    default:
        return NULL;
    }
}

