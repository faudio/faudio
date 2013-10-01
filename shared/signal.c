
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/signal.h>
#include <fa/pair/left.h>
#include <fa/priority_queue.h>
#include <fa/action.h>
#include <fa/buffer.h>
#include <fa/util.h>

// typedef fa_signal_t                 signal_t;
typedef fa_signal_unary_signal_t    fixpoint_t;
typedef fa_signal_unary_double_t    dunary_t;
typedef fa_signal_binary_double_t   dbinary_t;
typedef fa_action_t                 action_t;

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
            dunary_t           f;
            ptr_t           fd;
            signal_t        a;
        }                   lift;

        struct {
            string_t        name;
            dbinary_t       f;
            ptr_t           fd;
            signal_t        a;
            signal_t        b;
        }                   lift2;

        struct {
            fixpoint_t      f;
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

#define neg_bus(x) ((x + 1)*(-1))

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
    signal_t signal = new_signal(loop_signal);
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
    signal_t signal = new_signal(loop_signal);
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
    switch (signal->tag) {
    case time_signal:
        return fa_signal_time();

    case random_signal:
        return fa_signal_random();

    case constant_signal:
        return copy_constant(signal);

    case lift_signal:
        return copy_lift(signal);

    case lift2_signal:
        return copy_lift2(signal);

    case loop_signal:
        return copy_loop(signal);

    case delay_signal:
        return copy_delay(signal);

    case input_signal:
        return copy_input(signal);

    case output_signal:
        return copy_output(signal);

    default:
        assert(false);
    }
}

void fa_signal_destroy(fa_signal_t signal)
{
    // TODO
    delete_signal(signal);
}



// --------------------------------------------------------------------------------
// Introspection etc

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

fa_pair_t fa_signal_to_tree(fa_signal_t signal)
{
    switch (signal->tag) {
    case time_signal:
        return pair(string("time"), list());

    case random_signal:
        return pair(string("random"), list());

    case constant_signal:
        return pair(
                   fa_string_to_string(
                       fa_from_double(
                           constant_get(signal, value))),
                   list());

    case lift_signal:
        return pair(
                   lift_get(signal, name),
                   list(fa_signal_to_tree(lift_get(signal, a))));

    case lift2_signal:
        return pair(
                   lift_get(signal, name),
                   list(
                       fa_signal_to_tree(lift2_get(signal, a)),
                       fa_signal_to_tree(lift2_get(signal, b))));

    case input_signal:
        return pair(
                   concat(
                       string("input "),
                       fa_string_show(fa_from_int32(input_get(signal, c)))),
                   list());

    case output_signal:
        return pair(
                   concat(
                       string("output "),
                       fa_string_show(fa_from_int32(output_get(signal, c))),
                       string("[-"),
                       fa_string_show(fa_from_int32(output_get(signal, n))),
                       string("]")
                   ),

                   list(fa_signal_to_tree(output_get(signal, a))));

    default:
        assert(false);
    }
}

string_t draw_tree(pair_t value, string_t indent, bool is_last, string_t result)
{
    ptr_t  label    = fa_pair_first(value);
    list_t children = fa_pair_second(value);

    write_to(result, indent);

    if (is_last) {
        write_to(result, string("`- "));
        write_to(indent, string("   "));
    } else {
        write_to(result, string("+- "));
        write_to(indent, string("|  "));
    }

    write_to(result, fa_string_to_string(label));
    write_to(result, string("\n"));

    fa_for_each_last(x, children, last) {
        result = draw_tree(x, indent, last, result);
    }
    return result;
}

string_t fa_signal_draw_tree(fa_pair_t p)
{
    return draw_tree(p, string(""), true, string(""));
}


// --------------------------------------------------------------------------------
// Simplification

struct part {
    int o, d;
};
typedef struct part part_t;
void init_part(struct part *p)
{
    p->o = 0;
    p->d = 1;
}
void run_part(struct part *p, int *r, struct part *p2)
{
    *r = p->o;
    p2->o = p->o + p->d;
    p2->d = p->d;
}
void split_part(struct part *p, struct part *p2, struct part *p3)
{           
    p2->o = p->o;
    p2->d = p->d * 2;
    p3->o = p->d;
    p3->d = p->d * 2;
}

void run_part_neg(struct part *p, int *r, struct part *p2)
{
    run_part(p, r, p2);
    *r = neg_bus(*r);
}


// inline static
fa_signal_t simplify(part_t *part, fa_signal_t signal2)
{
    switch (signal2->tag) {

    case loop_signal: {
        fixpoint_t fix      = loop_get(signal2, f);
        ptr_t      fix_data = loop_get(signal2, fd);

        int channel;
        part_t part1;
        // Allocate one bus channel for the delay
        run_part_neg(part, &channel, &part1);

        signal_t input          = fa_signal_input(channel);
        signal_t res            = simplify(&part1, fix(fix_data, input));

        return fa_signal_output(1, channel, res);
    }

    case delay_signal: {
        signal_t a              = delay_get(signal2, a);
        int samples             = delay_get(signal2, n);

        int channel;
        part_t part1;
        // Allocate one bus channel for the delay
        run_part_neg(part, &channel, &part1);

        signal_t input          = fa_signal_input(channel);
        signal_t output         = fa_signal_output(samples, channel, simplify(&part1, a));

        return fa_signal_former(input, output);
    }

    case lift_signal: {
        string_t    name        = lift_get(signal2, name);
        dunary_t    func        = lift_get(signal2, f);
        ptr_t       func_data   = lift_get(signal2, fd);

        signal_t a              = simplify(part, lift_get(signal2, a));
        return fa_signal_lift(name, func, func_data, a);
    }

    case lift2_signal: {
        string_t    name        = lift2_get(signal2, name);
        dbinary_t   func        = lift2_get(signal2, f);
        ptr_t       func_data   = lift2_get(signal2, fd);

        part_t part1;
        part_t part2;
        // Split the channel partition
        split_part(part, &part1, &part2);

        signal_t a              = simplify(&part1, lift2_get(signal2, a));
        signal_t b              = simplify(&part2, lift2_get(signal2, b));

        return fa_signal_lift2(name, func, func_data, a, b);
    }

    case output_signal: {
        int samples             = output_get(signal2, n);
        int channel             = output_get(signal2, c);

        signal_t a              = simplify(part, output_get(signal2, a));

        return fa_signal_output(samples, channel, a);
    }

    default:
        return fa_copy(signal2);
    }
}
fa_signal_t fa_signal_simplify(fa_signal_t signal2)
{
    part_t p;
    init_part(&p);
    return simplify(&p, signal2);
}

// --------------------------------------------------------------------------------
// Running

typedef struct {
    double *inputs;     // Current input values
    double *buses;      // Current and future bus values
    ptr_t   controls;   // Current control values (list of actions)

    int count;          // Number of processed samples
    double rate;        // Sample rate

}  _state_t;
typedef _state_t *state_t;


double  kRate           = 44100;
long    kMaxInputs      = 128;
long    kMaxBuses       = 64;
long    kMaxDelay       = (44100 * 5);

state_t new_state()
{
    srand(time(NULL));  // TODO localize
    state_t state = fa_malloc(sizeof(_state_t));

    state->inputs   = fa_malloc(kMaxInputs              * sizeof(double));
    state->buses    = fa_malloc(kMaxBuses * kMaxDelay   * sizeof(double));
    memset(state->inputs,   0, kMaxInputs               * sizeof(double));
    memset(state->buses,    0, kMaxBuses * kMaxDelay    * sizeof(double));

    state->controls     = NULL;
    state->count        = 0;
    state->rate         = kRate;

    return state;
}

void delete_state(state_t state)
{
    fa_free(state->inputs);
    fa_free(state->buses);
    fa_free(state);
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

void    write_bus(int n, int c, double x, state_t state);
double  read_bus(int c, state_t state);
double  read_actual_input(int c, state_t state);
void write_actual_input(int c, double x, state_t state);

inline static
double read_samp(int c, state_t state)
{
    return (c >= 0) ? read_actual_input(c, state) : read_bus(neg_bus(c), state);
}

inline static
void write_samp(int n, int c, double x, state_t state)
{
    // write_bus(n, neg_bus(c), x, state);
    if (c >= 0) {
        write_actual_input(c, x, state);
    } else {
        write_bus(n, neg_bus(c), x, state);
    }
}

// inline static
void inc_state(state_t state)
{
    state->count++;
}

void reset_controls(state_t state)
{
    state->controls = NULL;
}

void push_control(ptr_t x, state_t state)
{
    // printf("Pushed %d %s\n", c, unstring(fa_string_show(x)));

    if (!state->controls) {
        state->controls = fa_list_single(x);
    } else {
        state->controls = fa_list_dcons(x, state->controls);
    }
}


//----------
// Internal state stuff

int buffer_pointer(state_t state)
{
    return state->count % kMaxDelay;
}

int index_bus(int n, int c)
{
    return c * kMaxDelay + n;
}

double read_actual_input(int c, state_t state)
{
    return state->inputs[c];
}

void write_actual_input(int c, double x, state_t state)
{
    state->inputs[c] = x;
}   

double read_bus(int c, state_t state)
{
    int bp = buffer_pointer(state);
    return state->buses[index_bus(bp, c)];
}

void write_bus(int n, int c, double x, state_t state)
{
    int bp = buffer_pointer(state);
    state->buses[index_bus((bp + n) % kMaxDelay, c)] = x;
}

//----------


/**
    Step over a sample.
 */
// inline static
double step(signal_t signal, state_t state)
{
    switch (signal->tag) {

    case time_signal: {
        return state_time(state);
    }

    case random_signal: {
        return state_random(state);
    }

    case constant_signal: {
        return constant_get(signal, value);
    }

    case lift_signal: {
        dunary_t f      = lift_get(signal, f);
        ptr_t    fd     = lift_get(signal, fd);
        signal_t a      = lift_get(signal, a);
        double   xa     = step(a, state);
        return f(fd, xa);
    }

    case lift2_signal: {
        dbinary_t   f   = lift2_get(signal, f);
        ptr_t       fd  = lift2_get(signal, fd);
        signal_t a      = lift2_get(signal, a);
        signal_t b      = lift2_get(signal, b);
        double   xa     = step(a, state);
        double   xb     = step(b, state);
        return f(fd, xa, xb);
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

#define USED(X) X = *(&X)
// No allocation in loop
// Use ringbuffers for transfer

void fa_signal_run(int n, list_t controls, signal_t a, double *output)
{
    priority_queue_t controls2 = priority_queue();
    fa_for_each(x, controls) {
        fa_priority_queue_insert(fa_pair_left_from_pair(x), controls2);
    }
    
    state_t state = new_state();
    signal_t a2 = fa_signal_simplify(a);
    // TODO optimize
    // TODO verify

    // TODO progress monitor
    for (int i = 0; i < n; ++ i) {
        
        reset_controls(state);

        while (1) {
            pair_t x = fa_priority_queue_peek(controls2);
            if (!x) break;
            
            time_t   time        = fa_pair_first(x);
            action_t action      = fa_pair_second(x);

            int timeSamp = ( ((double) fa_time_to_milliseconds(time)) / 1000.0 ) * 44100; // TODO

            if (timeSamp <= state->count) {

                // printf("Time: %s\nAction: %s\n\n",       
                //     unstring(fa_string_show(time)),
                //     // unstring(fa_string_show(i32(timeSamp))), 
                //     unstring(fa_string_show(action)));

                // printf("Time: %s\nAction: %i %lf\n\n",       
                //     unstring(fa_string_show(time)),
                //     // unstring(fa_string_show(i32(timeSamp))), 
                //     fa_action_set_channel(action),
                //     fa_action_set_value(action)
                //     );

                // TODO other action types
                int ch = fa_action_set_channel(action);
                double v = fa_action_set_value(action);
                write_samp(0, ch, v, state);
                
                fa_priority_queue_pop(controls2);
            } else {
                break;
            }
        }
        output[i] = step(a2, state);
        inc_state(state);
    }
    delete_state(state);
}

void fa_signal_print(int n, list_t controls, signal_t a)
{
    buffer_t b = fa_signal_run_buffer(n, controls, a);
    for (size_t i = 0; i < fa_buffer_size(b); ++i) {
        double x = fa_buffer_get_double(b, i);
        printf("%3ld: %4f\n", (long) i, x);
    }
    fa_destroy(b);
}

buffer_t fa_signal_run_buffer(int n, list_t controls, signal_t a)
{
    buffer_t b = fa_buffer_create(n * sizeof(double));
    fa_signal_run(n, controls, a, fa_buffer_unsafe_address(b));
    return b;
}

ptr_t fa_signal_run_file(int n, list_t controls, signal_t a, string_t path)
{
    buffer_t b = fa_signal_run_buffer(n, controls, a);
    ptr_t res = fa_buffer_write_audio(path, 1, b); // TODO number of channels
    fa_destroy(b);
    return res;
}




// --------------------------------------------------------------------------------
// Derived signals


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


inline static double _play(ptr_t buffer, double i)
{                 
    size_t size = fa_buffer_size(buffer);
    uint64_t j = ((uint64_t) i) % (size/sizeof(double));
    return fa_buffer_get_double(buffer, j);
}
fa_signal_t fa_signal_play(fa_buffer_t buffer, fa_signal_t i)
{                         
    return fa_signal_lift(string("play"), _play, buffer, i);
}

inline static double _record(ptr_t buffer, double i, double x)
{
    size_t size = fa_buffer_size(buffer);
    uint64_t j = ((uint64_t) i) % (size/sizeof(double));
    fa_buffer_set_double(buffer, j, x);
    return x;
}
fa_signal_t fa_signal_record(fa_buffer_t buffer, fa_signal_t i, fa_signal_t x)
{
    return fa_signal_lift2(string("record"), _record, buffer, i, x);
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
inline static double _cos(ptr_t _, double x)
{
    return cos(x);
}
fa_signal_t fa_signal_cos(fa_signal_t a)
{
    return fa_signal_lift(string("cos"), _cos, NULL, a);
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

string_t signal_show(ptr_t a)
{
    return fa_signal_draw_tree(fa_signal_to_tree(fa_signal_simplify(a)));
}

fa_ptr_t signal_impl(fa_id_t interface)
{
    static fa_copy_t signal_copy_impl
        = { signal_copy };
    static fa_destroy_t signal_destroy_impl
        = { signal_destroy };
    static fa_string_show_t signal_show_impl
        = { signal_show };

    switch (interface) {
    case fa_copy_i:
        return &signal_copy_impl;

    case fa_destroy_i:
        return &signal_destroy_impl;

    case fa_string_show_i:
        return &signal_show_impl;

    default:
        return NULL;
    }
}

