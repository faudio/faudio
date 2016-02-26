
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
#include <fa/file_buffer.h>
#include <fa/clock.h>
#include <fa/dynamic.h>
#include <fa/util.h>
#include <fa/midi/message.h>
#include <pthread.h> // temp

#include "signal.h"
#include "signal_internal.h"
#include "action_internal.h"

#ifndef _WIN32
#include "../platform/macosx/vst.h"
#endif

typedef fa_signal_custom_processor_t   *custom_proc_t;
typedef fa_signal_unary_signal_t        fixpoint_t;
typedef fa_signal_unary_double_t        fa_dunary_t;
typedef fa_signal_binary_double_t       fa_dbinary_t;
typedef fa_action_t                     action_t;

struct _fa_signal_t {

    fa_impl_t               impl;

    enum {
        time_signal,
        random_signal,
        constant_signal,
        lift_signal,
        lift2_signal,
        loop_signal,
        delay_signal,
        custom_signal,
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
            fa_string_t     name;
            fa_dunary_t     function;
            fa_ptr_t        data;
            fa_signal_t     a;
        }                   lift;

        struct {
            fa_string_t     name;
            fa_dbinary_t    function;
            fa_ptr_t        data;
            fa_signal_t     a;
            fa_signal_t     b;
        }                   lift2;

        struct {
            fixpoint_t      function;
            fa_ptr_t        data;
        } loop;

        struct {
            int             n;
            fa_signal_t     a;
        } delay;

        struct {
            custom_proc_t   proc;
            fa_signal_t     a;
        }                   custom;

        struct {
            int             c;
            fa_ptr_t        proc; // Custom processor (if any)
        }                   input;

        struct {
            int             n;
            int             c;
            fa_ptr_t        proc; // Custom processor (if any)
            fa_signal_t     a;
        }                   output;
    }                       fields;

};

inline static fa_signal_t new_signal(int tag)
{
    fa_ptr_t signal_impl(fa_id_t interface);

    fa_signal_t s = fa_new(signal);
    s->impl = &signal_impl;
    s->tag  = tag;
    return s;
}

inline static void delete_signal(fa_signal_t signal)
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
#define is_custom(v)        (v->tag == custom_signal)
#define is_input(v)         (v->tag == input_signal)
#define is_output(v)        (v->tag == output_signal)

#define time_get(v,f)       v->fields.time.f
#define random_get(v,f)     v->fields.random.f
#define constant_get(v,f)   v->fields.constant.f
#define lift_get(v,f)       v->fields.lift.f
#define lift2_get(v,f)      v->fields.lift2.f
#define loop_get(v,f)       v->fields.loop.f
#define delay_get(v,f)      v->fields.delay.f
#define custom_get(v,f)     v->fields.custom.f
#define input_get(v,f)      v->fields.input.f
#define output_get(v,f)     v->fields.output.f

#define neg_bus(x) ((x + 1)*(-1))

// --------------------------------------------------------------------------------

fa_signal_t fa_signal_time()
{
    fa_signal_t signal = new_signal(time_signal);
    return signal;
}

fa_signal_t fa_signal_random()
{
    fa_signal_t signal = new_signal(random_signal);
    return signal;
}

fa_signal_t fa_signal_constant(double x)
{
    fa_signal_t signal = new_signal(constant_signal);
    constant_get(signal, value) = x;
    return signal;
}


fa_signal_t fa_signal_lift(fa_string_t n,
                           fa_signal_unary_double_t function,
                           fa_ptr_t data,
                           fa_signal_t a)
{
    fa_signal_t signal = new_signal(lift_signal);
    lift_get(signal, name)      = n;
    lift_get(signal, function)  = function;
    lift_get(signal, data)      = data;
    lift_get(signal, a)         = a;
    return signal;
}


fa_signal_t fa_signal_lift2(fa_string_t n,
                            fa_signal_binary_double_t function,
                            fa_ptr_t data,
                            fa_signal_t a,
                            fa_signal_t b)
{
    fa_signal_t signal = new_signal(lift2_signal);
    lift2_get(signal, name)     = n;
    lift2_get(signal, function) = function;
    lift2_get(signal, data)     = data;
    lift2_get(signal, a)        = a;
    lift2_get(signal, b)        = b;
    return signal;
}

fa_signal_t fa_signal_loop(fa_signal_unary_signal_t function, fa_ptr_t data)
{
    if (kVectorMode) {
        fa_warn(fa_string("Loop not supported in vector mode"));
        assert(false && "Loop not supported in vector mode");
    }

    fa_signal_t signal = new_signal(loop_signal);
    loop_get(signal, function)  = function;
    loop_get(signal, data)      = data;
    return signal;
}


fa_signal_t fa_signal_delay(int n, fa_signal_t a)
{
    if (kVectorMode) {
        fa_warn(fa_string("Delay not supported in vector mode"));
        assert(false && "Delay not supported in vector mode");
    }

    fa_signal_t signal = new_signal(delay_signal);
    delay_get(signal, n)  = n;
    delay_get(signal, a)  = a;
    return signal;
}

fa_signal_t fa_signal_custom(custom_proc_t proc, fa_signal_t a)
{
    fa_signal_t signal = new_signal(custom_signal);
    custom_get(signal, proc)        = proc;
    custom_get(signal, a)           = a;
    return signal;
}

fa_signal_t fa_signal_input(int c)
{
    fa_signal_t signal = new_signal(input_signal);
    input_get(signal, c)  = c;
    input_get(signal, proc) = NULL;
    return signal;
}
fa_signal_t fa_signal_input_with_custom(custom_proc_t proc, int c)
{
    fa_signal_t signal = new_signal(input_signal);
    input_get(signal, c)  = c;
    input_get(signal, proc) = proc;
    return signal;
}

fa_signal_t fa_signal_output(int n, int c, fa_signal_t a)
{
    fa_signal_t signal = new_signal(output_signal);
    output_get(signal, n)  = n;
    output_get(signal, c)  = c;
    output_get(signal, a)  = a;
    output_get(signal, proc)  = NULL;
    return signal;
}
fa_signal_t fa_signal_output_with_custom(custom_proc_t proc, int n, int c, fa_signal_t a)
{
    fa_signal_t signal = new_signal(output_signal);
    output_get(signal, n)  = n;
    output_get(signal, c)  = c;
    output_get(signal, a)  = a;
    output_get(signal, proc)  = proc;
    return signal;
}

fa_signal_t copy_constant(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(constant_signal);
    constant_get(signal, value) = constant_get(signal2, value);
    return signal;
}

fa_signal_t copy_lift(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(lift_signal);
    lift_get(signal, name)      = lift_get(signal2, name);
    lift_get(signal, function)  = lift_get(signal2, function);
    lift_get(signal, data)      = lift_get(signal2, data);
    lift_get(signal, a)         = lift_get(signal2, a);
    return signal;
}

fa_signal_t copy_lift2(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(lift2_signal);
    lift2_get(signal, name)     = lift2_get(signal2, name);
    lift2_get(signal, function) = lift2_get(signal2, function);
    lift2_get(signal, data)     = lift2_get(signal2, data);
    lift2_get(signal, a)        = lift2_get(signal2, a);
    lift2_get(signal, b)        = lift2_get(signal2, b);
    return signal;
}

fa_signal_t copy_loop(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(loop_signal);
    loop_get(signal, function)  = loop_get(signal2, function);
    loop_get(signal, data)      = loop_get(signal2, data);
    return signal;
}

fa_signal_t copy_delay(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(delay_signal);
    delay_get(signal, n)  = delay_get(signal2, n);
    delay_get(signal, a)  = delay_get(signal2, a);
    return signal;
}


fa_signal_t copy_custom(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(custom_signal);
    custom_get(signal, proc)        = custom_get(signal2, proc);
    custom_get(signal, a)           = custom_get(signal2, a);
    return signal;
}


fa_signal_t copy_input(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(input_signal);
    input_get(signal, c)  = input_get(signal2, c);
    input_get(signal, proc)  = input_get(signal2, proc);
    return signal;
}

fa_signal_t copy_output(fa_signal_t signal2)
{
    fa_signal_t signal = new_signal(output_signal);
    output_get(signal, n)  = output_get(signal2, n);
    output_get(signal, c)  = output_get(signal2, c);
    output_get(signal, a)  = output_get(signal2, a);
    output_get(signal, proc)  = output_get(signal2, proc);
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

    case custom_signal:
        return copy_custom(signal);

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

bool fa_signal_is_constant(fa_signal_t a)
{
    if (is_constant(a)) {
        return true;
    }

    if (is_lift(a)) {
        return fa_signal_is_constant(lift2_get(a, a));
    }

    if (is_lift2(a)) {
        return fa_signal_is_constant(lift2_get(a, a))
               && fa_signal_is_constant(lift2_get(a, b));
    }

    return false;
}

bool fa_signal_is_variable(fa_signal_t a)
{
    return !fa_signal_is_constant(a);
}

int fa_signal_signal_node_count(fa_signal_t a)
{
    assert(false && "Not implemented");
}

fa_pair_t fa_signal_to_tree(fa_signal_t signal)
{
    switch (signal->tag) {

    case time_signal:
        return fa_pair_create(fa_string("time"), fa_empty());

    case random_signal:
        return fa_pair_create(fa_string("random"), fa_empty());

    case constant_signal: {
        double value = constant_get(signal, value);
        return fa_pair_create(fa_string_to_string(fa_from_double(value)), fa_empty());
    }

    case lift_signal: {
        fa_pair_t tree = fa_signal_to_tree(lift_get(signal, a));
        return fa_pair_create(lift_get(signal, name), list(tree));
    }

    case lift2_signal: {
        fa_pair_t tree1 = fa_signal_to_tree(lift2_get(signal, a));
        fa_pair_t tree2 = fa_signal_to_tree(lift2_get(signal, b));
        return fa_pair_create(lift_get(signal, name), list(tree1, tree2));
    }

    case input_signal:
        return fa_pair_create(
                   dconcat(
                       fa_string("input "),
                       input_get(signal, proc)
                       ? fa_string_format_integral("%lu@", (long) input_get(signal, proc))
                       : fa_string(""),
                       fa_string_format_integral("%d", input_get(signal, c))),
                   fa_empty());

    case output_signal:
        return fa_pair_create(
                   dconcat(
                       dconcat(
                           fa_string("output "),
                           output_get(signal, proc)
                           ? fa_string_format_integral("%lu@", (long) output_get(signal, proc))
                           : fa_string("")
                       ),
                       fa_string_show(fa_from_int32(output_get(signal, c))),
                       fa_string("[-"),
                       fa_string_show(fa_from_int32(output_get(signal, n))),
                       fa_string("]")
                   ),

                   list(fa_signal_to_tree(output_get(signal, a))));

    default:
        assert(false);
    }
}

fa_string_t draw_tree(fa_pair_t value, fa_string_t indent, bool is_last, fa_string_t result)
{
    fa_ptr_t  label    = fa_pair_first(value);
    fa_list_t children = fa_pair_second(value);

    fa_dwrite_string(result, fa_copy(indent));

    if (is_last) {
        fa_dwrite_string(result, fa_string("`- "));
        fa_dwrite_string(indent, fa_string("   "));
    } else {
        fa_dwrite_string(result, fa_string("+- "));
        fa_dwrite_string(indent, fa_string("|  "));
    }

    fa_dwrite_string(result, fa_string_to_string(label));
    fa_dwrite_string(result, fa_string("\n"));

    fa_for_each_last(x, children, last) {
        result = draw_tree(x, fa_copy(indent), last, result);
    }
	fa_destroy(indent);
    return result;
}

fa_string_t fa_signal_draw_tree(fa_pair_t p)
{
    return draw_tree(p, fa_string(""), true, fa_string(""));
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



// TODO
int get_buffer_range(part_t *part,
                     part_t *newPart,
                     fa_string_t name,
                     int is,
                     int os)
{
    newPart->o = part->o;
    newPart->d = part->d;
    return 16;
}

// inline static
fa_signal_t simplify(part_t *part, fa_list_t *procs, fa_signal_t signal2)
{
    switch (signal2->tag) {

    case loop_signal: {
        fixpoint_t fix      = loop_get(signal2, function);
        fa_ptr_t   fix_data = loop_get(signal2, data);

        int channel;
        part_t part1;
        // Allocate one bus channel for the delay
        run_part_neg(part, &channel, &part1);

        fa_signal_t input          = fa_signal_input(channel);
        fa_signal_t res            = simplify(&part1, procs, fix(fix_data, input));

        return fa_signal_output(1, channel, res);
    }

    case delay_signal: {
        fa_signal_t a              = delay_get(signal2, a);
        int samples                = delay_get(signal2, n);

        int channel;
        part_t part1;
        // Allocate one bus channel for the delay
        run_part_neg(part, &channel, &part1);

        fa_signal_t input          = fa_signal_input(channel);
        fa_signal_t output         = fa_signal_output(samples, channel, simplify(&part1, procs, a));

        return fa_signal_former(input, output);
    }

    case custom_signal: {
        fa_signal_t a              = simplify(part, procs, custom_get(signal2, a));

        custom_proc_t proc = custom_get(signal2, proc);
        // Push to processor list
        *procs = fa_list_dcons(proc, *procs);
        return a;
    }

    case lift_signal: {
        fa_string_t    name        = lift_get(signal2, name);
        fa_dunary_t    func        = lift_get(signal2, function);
        fa_ptr_t       func_data   = lift_get(signal2, data);

        fa_signal_t a              = simplify(part, procs, lift_get(signal2, a));
        return fa_signal_lift(name, func, func_data, a);
    }

    case lift2_signal: {
        fa_string_t    name        = lift2_get(signal2, name);
        fa_dbinary_t   func        = lift2_get(signal2, function);
        fa_ptr_t       func_data   = lift2_get(signal2, data);

        part_t part1;
        part_t part2;
        // Split the channel partition
        split_part(part, &part1, &part2);

        fa_signal_t a              = simplify(&part1, procs, lift2_get(signal2, a));
        fa_signal_t b              = simplify(&part2, procs, lift2_get(signal2, b));

        return fa_signal_lift2(name, func, func_data, a, b);
    }

    case output_signal: {
        int samples             = output_get(signal2, n);
        int channel             = output_get(signal2, c);
        fa_ptr_t proc           = output_get(signal2, proc);

        fa_signal_t a              = simplify(part, procs, output_get(signal2, a));

        fa_signal_t res = fa_signal_output(samples, channel, a);
        output_get(res, proc) = proc;
        return res;
    }

    default:
        return fa_copy(signal2);
    }
}
fa_signal_t fa_signal_simplify(fa_signal_t signal2)
{
    part_t part;
    init_part(&part);
    fa_list_t procs = fa_empty();
    return simplify(&part, &procs, signal2);
}

/*
    fa_ptr_t offset = lookup_proc_offset(proc_map, (intptr_t) x);
    if (offset) {
        ((custom_proc_t) x)->channel_offset = fa_peek_int64(offset);
    } else {
        ((custom_proc_t) x)->channel_offset = 0;
        fa_warn(fa_string("Could not find processor offset"));
    }

*/

// fa_ptr_t lookup_proc_offset(fa_map_t proc_map, intptr_t x);


// Map RawPtr Int64
fa_signal_t fa_signal_route_processors(fa_map_t proc_map, fa_signal_t signal2)
{
    switch (signal2->tag) {

    case loop_signal: {
        assert(false && "Must simplify before calling Signal.routeProcessors");
    }

    case delay_signal: {
        assert(false && "Must simplify before calling Signal.routeProcessors");
    }

    case custom_signal: {
        assert(false && "Must simplify before calling Signal.routeProcessors");
    }

    case lift_signal: {
        fa_string_t    name        = lift_get(signal2, name);
        fa_dunary_t    func        = lift_get(signal2, function);
        fa_ptr_t       func_data   = lift_get(signal2, data);
        fa_signal_t a              = fa_signal_route_processors(proc_map, lift_get(signal2, a));
        return fa_signal_lift(name, func, func_data, a);
    }

    case lift2_signal: {
        fa_string_t    name        = lift2_get(signal2, name);
        fa_dbinary_t   func        = lift2_get(signal2, function);
        fa_ptr_t       func_data   = lift2_get(signal2, data);
        fa_signal_t a              = fa_signal_route_processors(proc_map, lift2_get(signal2, a));
        fa_signal_t b              = fa_signal_route_processors(proc_map, lift2_get(signal2, b));
        return fa_signal_lift2(name, func, func_data, a, b);
    }

    case output_signal: {
        int samples                 = output_get(signal2, n);
        int channel                 = output_get(signal2, c);
        fa_ptr_t proc               = output_get(signal2, proc);
        // TODO channel
        fa_signal_t a               = fa_signal_route_processors(proc_map, output_get(signal2, a));

        if (proc) {
            fa_ptr_t offset = lookup_proc_offset(proc_map, (intptr_t) proc);

            if (offset) {
                int64_t offset2 = fa_peek_int64(offset);
                channel += offset2;
            } else {
                fa_warn(fa_string_format_integral("Could not find processor offset: %lu", (unsigned long) proc));
            }
        }

        return fa_signal_output(samples, channel, a);
    }

    case input_signal: {
        int channel                 = input_get(signal2, c);
        fa_ptr_t proc               = input_get(signal2, proc);
        // TODO channel

        if (proc) {
            fa_ptr_t offset = lookup_proc_offset(proc_map, (intptr_t) proc);

            if (offset) {
                int64_t offset2 = fa_peek_int64(offset);
                channel += offset2;
            } else {
                fa_warn(fa_string_format_integral("Could not find processor offset: %lu", (unsigned long) proc));
            }
        }

        return fa_signal_input(channel);
    }

    default:
        return fa_copy(signal2);
    }
}

fa_signal_t fa_signal_doptimize(fa_signal_t signal)
{
    return signal;
}
fa_signal_t fa_signal_dverify(fa_signal_t signal)
{
    // TODO assert depth <= kMaxSignalTreeDepth
    return signal;
}

fa_list_t fa_signal_get_procs(fa_signal_t signal2)
{
    part_t part;
    init_part(&part);
    fa_list_t procs = fa_empty();
    simplify(&part, &procs, signal2);
    return procs;
}

// --------------------------------------------------------------------------------
// Running

#define max_delay(state) ((long) (state->rate * kMaxDelaySeconds))

/*  Internal DSP state.
    Local to a real-time stream or non-real-time "run" of a signal.

    Contains all "input" buffers (actually both input and output) as well as "buses", used
    internally for loop and delay.

    The input buffer contains up to kMaxVectorSize values for each channel and are non-interleaved.
    The bus channel contains up to max_delay(state) values for each channel and are non-interleaved.

    This layout is used regardless of whether vector processing is enabled or not, so allocation and indexing
    is unaffected by this change. In non-vector mode, only the first index of an I/O bus is used. In vector
    mode up to kMaxVectorSize indices may be used, depending on current vector size settings and number of
    samples being delivered by the underlying system API.

 */
struct _state_t {
    double     *inputs;                 // Current input values (TODO should not be called inputs as they are also outputs...)
    double     *buses;                  // Current and future bus values

    uint64_t    count;                  // Number of processed samples
    double      rate;                   // Sample rate (immutable during processing)
    double      speed;
    double      elapsed_time;

    int           custom_proc_count;
    custom_proc_t custom_procs[kMaxCustomProcs];      // Array of custom processors

};

state_t new_state(int sample_rate)
{
    // TODO use a single random generator for whole signal, see below
    srand(time(NULL));
    state_t state = fa_new_struct(_state_t);

    state->count              = 0;
    state->rate               = sample_rate;
    state->speed              = 1.0;
    state->elapsed_time       = 0.0;
    state->custom_proc_count  = 0;

    state->inputs   = fa_malloc(kMaxInputs * kMaxVectorSize * sizeof(double));
    state->buses    = fa_malloc(kMaxBuses * max_delay(state)   * sizeof(double));
    memset(state->inputs,   0, kMaxInputs * kMaxVectorSize * sizeof(double));
    memset(state->buses,    0, kMaxBuses * max_delay(state)    * sizeof(double));

    return state;
}

void add_custom_proc(custom_proc_t proc, state_t state)
{
    assert(state->custom_proc_count < kMaxCustomProcs && "Too many custom processors");
    state->custom_procs[state->custom_proc_count] = proc;
    state->custom_proc_count++;
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
    // TODO use a single random generator for whole signal, see above
    return ((double)rand() / (double)RAND_MAX) * 2 - 1;
}

inline static
double state_time(state_t state)
{
    return state->elapsed_time;
    // return state->count / state->rate;
}

inline static
double state_time_plus(state_t state, int n)
{
    return (state->count + n) / state->rate;
}

inline static double  read_input1(int c, state_t state);
inline static double  read_bus1(int c, state_t state);
inline static void    write_input1(int c, double x, state_t state);
inline static void    write_bus1(int n, int c, double x, state_t state);

inline static double  *read_input(int c, state_t state);
inline static double  *read_bus(int c, state_t state);
inline static double  *write_input(int c, state_t state);
inline static double  *write_bus(int n, int c, state_t state);

inline static
double read_samp1(int c, state_t state)
{
    return (c >= 0) ? read_input1(c, state) : read_bus1(neg_bus(c), state);
}

inline static
void write_samp1(int n, int c, double x, state_t state)
{
    if (c >= 0) {
        write_input1(c, x, state);
    } else {
        write_bus1(n, neg_bus(c), x, state);
    }
}

inline static
double *read_samp(int c, state_t state)
{
    return (c >= 0) ? read_input(c, state) : read_bus(neg_bus(c), state);
}

inline static
double *write_samp(int n, int c, state_t state)
{
    if (c >= 0) {
        return write_input(c, state);
    } else {
        return write_bus(n, neg_bus(c), state);
    }
}


// inline static
void inc_state1(state_t state)
{
    state->count++;
    state->elapsed_time += (1.0 / state->rate) * state->speed;
}

void inc_state(int n, state_t state)
{
    state->count += n;
    state->elapsed_time += ((double) n / state->rate) * state->speed;
}


//----------
// Internal state stuff

/* */
int buffer_pointer(state_t state)
{
    return state->count % max_delay(state);
}

int index_bus(int n, int c, state_t state)
{
    return c * max_delay(state) + n;
}

// a[n]  = *(a + n)
// &a[n] = a + n

double read_input1(int c, state_t state)
{
    return *read_input(c, state);
}

void write_input1(int c, double x, state_t state)
{
    *write_input(c, state) = x;
}

double read_bus1(int c, state_t state)
{
    return *read_bus(c, state);
}

void write_bus1(int n, int c, double x, state_t state)
{
    *write_bus(n, c, state) = x;
}


double *read_input(int c, state_t state)
{
    return state->inputs + (c * kMaxVectorSize);
}

double *write_input(int c, state_t state)
{
    return state->inputs + (c * kMaxVectorSize);
}

double *read_bus(int c, state_t state)
{
    int bp = buffer_pointer(state);
    return state->buses + (index_bus(bp, c, state));
}

double *write_bus(int n, int c, state_t state)
{
    int bp = buffer_pointer(state);
    return state->buses + (index_bus((bp + n) % max_delay(state), c, state));
}












/**
    Loops through the custom processors in a state_t or state2_t, running
    the respective rendering action.

    0 -> prepare
    1 -> run
    2 -> cleanup

 */
void run_custom_procs(custom_proc_when_t when, int count, state_t state)
{
    for (int i = 0; i < state->custom_proc_count; ++i) {

        custom_proc_t proc = state->custom_procs[i];
        // printf("Running custom proc %p!\n", proc);

        switch (when) {
        case custom_proc_before:
            proc->before(proc->data, kMaxVectorSize, (fa_signal_state_t *) state);
            break;

        case custom_proc_render:
            proc->render(proc->data, proc->channel_offset, count, (fa_signal_state_t *) state);
            break;

        case custom_proc_after:
            proc->after(proc->data, kMaxVectorSize, (fa_signal_state_t *) state);
            break;

        case custom_proc_destroy: {
            if (proc->destroy && proc->data) {
                proc->destroy(proc->data);
                proc->data = NULL;
            }

            break;
        }

        default:
            assert(false);
        }
    }
}

uint64_t last_count = 0;

// typedef void(* fa_signal_message_callback_t)(fa_ptr_t, fa_signal_name_t, fa_signal_message_t)
void custom_procs_send(state_t state, fa_string_t name, fa_ptr_t value)
{
    for (int i = 0; i < state->custom_proc_count; ++i) {
        custom_proc_t proc = state->custom_procs[i];

        if (proc->receive) {
            //printf("custom_procs_send, count: %llu  (diff: %llu)\n", state->count, state->count - last_count);
            last_count = state->count;
            proc->receive(proc->data, name, value);
        }
    }
}

void custom_procs_receive(state_t state, fa_signal_message_callback_t cb, fa_ptr_t data)
{
    // FIXME
    for (int i = 0; i < state->custom_proc_count; ++i) {
        custom_proc_t proc = state->custom_procs[i];

        if (proc->send) {
            proc->send(proc->data, cb, data);
        }
    }
}

/**
    Run a simple action.

    @param
        action  Action to run.
        state   State to run action on (for control updates and custom processor messages).
 */
fa_ptr_t run_simple_action(state_t state, action_t action)
{
    if (fa_action_is_compound(action)) {
        fa_warn(fa_string_dappend(fa_string("Compound action passed to Signal.runSimpleAction: "), fa_string_show(action)));
        return NULL;
    }
    //fa_slog_info("run_simple_action in thread ", fa_string_format_integral("%p", (long) fa_thread_current()));
    //printf("run_simple_action %p\n", pthread_self());

    if (fa_action_is_get(action)) {
        int ch = fa_action_get_channel(action);
        fa_signal_unary_double_t f = fa_action_get_function(action);
        fa_ptr_t ctxt = fa_action_get_data(action);
        double x = read_samp1(ch, state);
        f(ctxt, x); // Ignore result
        fa_action_destroy(action);
        return NULL;
    }

    if (fa_action_is_set(action)) {
        int ch = fa_action_set_channel(action);
        double v = fa_action_set_value(action);
        write_samp1(0, ch, v, state);
        fa_action_destroy(action);
        return NULL;
    }

    if (fa_action_is_accum(action)) {
        int ch = fa_action_accum_channel(action);
        fa_signal_unary_double_t f = fa_action_accum_function(action);
        fa_ptr_t ctxt = fa_action_accum_data(action);

        double x  = read_samp1(ch, state);
        double x2 = f(ctxt, x);

        write_samp1(0, ch, x2, state);
        
        fa_action_destroy(action);
        return NULL;
    }

    if (fa_action_is_send(action)) {
        fa_string_t name = fa_action_send_name(action);
        fa_ptr_t value = fa_action_send_value(action);
        //printf("timestamp: %llu\n", fa_action_timestamp(action));
        custom_procs_send(state, name, value);
        
        //printf("ref_count: %d\n", fa_action_ref_count(action));
        fa_action_destroy(action);
        
        return NULL;
    }

    fa_warn(fa_string_dappend(fa_string("Unknown simple action passed to Signal.runSimpleAction: "), fa_string_show(action)));
    fa_action_destroy(action);
    return NULL;
}









/**
    Step over a sample.
 */
double step(fa_signal_t signal, state_t state)
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
        fa_dunary_t    function  = lift_get(signal, function);
        fa_ptr_t       data      = lift_get(signal, data);
        fa_signal_t    a         = lift_get(signal, a);
        double      xa        = step(a, state);
        return function(data, xa);
    }

    case lift2_signal: {
        fa_dbinary_t   function  = lift2_get(signal, function);
        fa_ptr_t       data      = lift2_get(signal, data);
        fa_signal_t    a         = lift2_get(signal, a);
        fa_signal_t    b         = lift2_get(signal, b);
        double      xa        = step(a, state);
        double      xb        = step(b, state);
        return function(data, xa, xb);
    }

    case input_signal: {
        int         c         = input_get(signal, c);
        return read_samp1(c, state);
    }

    case output_signal: {
        int         n         = output_get(signal, n);
        int         c         = output_get(signal, c);
        fa_signal_t a         = output_get(signal, a);

        double      xa        = step(a, state);
        write_samp1(n, c, xa, state);
        return xa;
    }

    default:
        assert(false && "step: Strange signal");
    }

    assert(false);
}

/**
    Step over a vector of samples.
 */
void step_vector(fa_signal_t signal, state_t state, int count, double *out)
{
    switch (signal->tag) {

    case time_signal: {
        for (int i = 0; i < count; ++i) {
            out[i] = state_time_plus(state, i);
        }

        return;
    }

    case random_signal: {
        for (int i = 0; i < count; ++i) {
            out[i] = state_random(state);
        }

        return;
    }

    case constant_signal: {
        for (int i = 0; i < count; ++i) {
            out[i] = constant_get(signal, value);
        }

        return;
    }

    case lift_signal: {
        fa_dunary_t    function  = lift_get(signal, function);
        fa_ptr_t       data      = lift_get(signal, data);
        fa_signal_t    a         = lift_get(signal, a);
        step_vector(a, state, count, out);

        for (int i = 0; i < count; ++i) {
            out[i] = function(data, out[i]);
        }

        return;
    }

    case lift2_signal: {
        fa_dbinary_t   function  = lift2_get(signal, function);
        fa_ptr_t       data      = lift2_get(signal, data);
        fa_signal_t    a         = lift2_get(signal, a);
        fa_signal_t    b         = lift2_get(signal, b);

        // TODO is stack allocation efficient enough?
        double temp[count];
        step_vector(a, state, count, out);
        step_vector(b, state, count, temp);

        for (int i = 0; i < count; ++i) {
            out[i] = function(data, out[i], temp[i]);
        }

        return;
    }

    case input_signal: {
        int         c = input_get(signal, c);
        double    *xs = read_samp(c, state);

        for (int i = 0; i < count; ++i) {
            out[i] = xs[i];
        }

        return;
    }

    case output_signal: {
        int         n = output_get(signal, n);
        int         c = output_get(signal, c);
        fa_signal_t a = output_get(signal, a);

        step_vector(a, state, count, out);
        double *xs = write_samp(n, c, state);

        for (int i = 0; i < count; ++i) {
            xs[i] = out[i];
        }

        return;
    }

    default:
        assert(false && "step: Strange signal");
    }

    assert(false);
}

fa_ptr_t run_simple_action_(fa_ptr_t x, fa_ptr_t a, fa_ptr_t ignored_time)
{
    return run_simple_action(x, a);
}


inline static
fa_list_t fa_list_r(int64_t x, int64_t y)
{
    return x >= y ? fa_list_empty() : fa_list_cons(fa_from_int64(x), fa_list_r(1 + x, y));
}

inline static
fa_ptr_t _add32__pointer_list_to_custom_proc_map(fa_ptr_t x)
{
    return fa_add(x, fa_from_int64(32));
}
inline static
fa_ptr_t _times4__pointer_list_to_custom_proc_map(fa_ptr_t x)
{
    return fa_multiply(x, fa_from_int64(4));
}

fa_map_t pointer_list_to_custom_proc_map(fa_list_t xs)
{
    return fa_map_from_list(fa_list_zip(
                                fa_list_remove_duplicates(
                                    // fa_list_map(apply1, fa_from_int64, xs)
                                    xs
                                ),
                                fa_list_map(apply1, _add32__pointer_list_to_custom_proc_map,
                                            fa_list_map(apply1, _times4__pointer_list_to_custom_proc_map,
                                                        fa_list_r(0, fa_list_length(xs))))
                            ));
}

fa_ptr_t lookup_proc_offset(fa_map_t proc_map, intptr_t x)
{
    return fa_map_dget(fa_from_int64(x), proc_map);
}

inline static
fa_priority_queue_t list_to_queue(fa_list_t controls_)
{
    fa_priority_queue_t controls = fa_priority_queue();
    fa_for_each(x, controls_) {
        fa_priority_queue_insert(fa_pair_left_from_pair(x), controls);
    }
    return controls;
}

/**
    Traverse processors and build a map from local to global buses.
    Side effect: mutates channel_offset in each proc to the calulated index.
 */
fa_map_t build_proc_map(fa_list_t procs)
{
    fa_list_t procs2 = fa_list_empty();
    fa_for_each(x, procs) {
        // printf("%lu\n", (unsigned long) x);
        procs2 = fa_list_dcons(fa_from_int64((int64_t) x), procs2);
    }

    // XXX Before this, inform custom procs of their offset
    // we have all the procs in a list (as raw pointer?)
    // Now remove duplicates, then build a map (ProcId => BusIndexOffset)
    fa_map_t proc_map = pointer_list_to_custom_proc_map(procs2);

    fa_for_each(x, procs) {
        // printf("%lu\n", (unsigned long) x);
        fa_ptr_t offset = lookup_proc_offset(proc_map, (intptr_t) x);

        if (offset) {
            ((custom_proc_t) x)->channel_offset = fa_peek_int64(offset);
        } else {
            ((custom_proc_t) x)->channel_offset = 0;
            fa_warn(fa_string_format_integral("Could not find processor offset: %lu", (unsigned long) x));
        }
    }
    return proc_map;
}

void fa_signal_run(int count, fa_list_t controls_, fa_signal_t a, double *output)
{
    {
        state_t             state    = new_state(kDefSampleRate); // TODO other sample rates
        fa_priority_queue_t controls = list_to_queue(controls_);

        fa_list_t procs = fa_signal_get_procs(a);
        fa_map_t  proc_map = build_proc_map(procs);
        fa_for_each(x, procs) {
            add_custom_proc(x, state);
        }

        // XXX Before this, replace "local" buses with "global" (for custom procs)
        a = fa_signal_simplify(a);

        fa_inform(fa_string_dappend(fa_string("    Signal Tree 1: \n"), fa_string_show(a)));
        a = fa_signal_route_processors(proc_map, a);

        fa_inform(fa_string_dappend(fa_string("    Signal Tree 2: \n"), fa_string_show(a)));
        a = fa_signal_doptimize(a);
        a = fa_signal_dverify(a);

        {
            run_custom_procs(custom_proc_before, 0, state);

            for (int i = 0; i < count; ++ i) {
                fa_time_t now = fa_milliseconds((double) state->count / (double) state->rate * 1000.0);
                run_actions(controls, now, run_simple_action_, state);

                run_custom_procs(custom_proc_render, 1, state);
                output[i] = step(a, state);
                inc_state1(state);
                fa_destroy(now);
            }

            run_custom_procs(custom_proc_after, 0, state);
        }
        delete_state(state);
    }
}

fa_buffer_t fa_signal_run_buffer(int n, fa_list_t controls, fa_signal_t a)
{
    fa_buffer_t b = fa_buffer_create(n * sizeof(double));
    fa_signal_run(n, controls, a, fa_buffer_unsafe_address(b));
    return b;
}

void fa_signal_print(int n, fa_list_t controls, fa_signal_t a)
{
    fa_buffer_t b = fa_signal_run_buffer(n, controls, a);

    for (size_t i = 0; (i * sizeof(double)) < fa_buffer_size(b); ++i) {
        double x = fa_buffer_get_double(b, i);
        printf("%3ld: %4f\n", (long) i, x);
    }

    fa_destroy(b);
}

fa_ptr_t fa_signal_run_file(int n, fa_list_t controls, fa_signal_t a, fa_string_t path)
{
    fa_buffer_t b = fa_signal_run_buffer(n, controls, a);
    fa_ptr_t res = fa_buffer_write_audio(path, b); // TODO number of channels
    fa_destroy(b);
    return res;
}






















































// --------------------------------------------------------------------------------
// Derived signals


inline static double _former(fa_ptr_t _, double x, double y)
{
    return x;
}
fa_signal_t fa_signal_former(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(fa_string("former"), _former, NULL, a, b);
}

inline static double _latter(fa_ptr_t _, double x, double y)
{
    return y;
}
fa_signal_t fa_signal_latter(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(fa_string("latter"), _latter, NULL, a, b);
}



inline static double _impulse(fa_ptr_t _, double x)
{
    return (x == 0) ? 1 : 0;
}
fa_signal_t fa_signal_impulse()
{
    return fa_signal_lift(fa_string("mkImp"), _impulse, NULL, fa_signal_time());
}

/*
    (defun signal-counter ()
      (- (signal-loop* (lambda (x) (+ x 1))) 1))

*/
inline static fa_signal_t _fix_counter(fa_ptr_t _, fa_signal_t x)
{
    return fa_signal_add(x, fa_signal_constant(1));
}
fa_signal_t fa_signal_counter()
{
    return fa_signal_add(fa_signal_loop(_fix_counter, NULL), fa_signal_constant(-1));
}


inline static double _impulses(fa_ptr_t n, double x)
{
    size_t n2 = (size_t) n;
    int x2 = (int) x;
    return (x2 % n2) == 0 ? 1 : 0;
}
fa_signal_t fa_signal_impulses(size_t n)
{
    return fa_signal_lift(fa_string("mkImps"), _impulses, (fa_ptr_t) n, fa_signal_counter());
}


fa_signal_t fa_signal_line(double x)
{
    double tau = 2 * 3.141592653589793;
    return fa_signal_multiply(fa_signal_time(), fa_signal_constant(x * tau));
}


inline static double _play(fa_ptr_t buffer, double i)
{
    size_t size = fa_buffer_size(buffer);
    uint64_t j = ((uint64_t) i) % (size / sizeof(double));
    return fa_buffer_get_double(buffer, j);
}
fa_signal_t fa_signal_play(fa_buffer_t buffer, fa_signal_t i)
{
    return fa_signal_lift(fa_string("play"), _play, buffer, i);
}

inline static double _record(fa_ptr_t buffer, double i, double x)
{
    size_t size = fa_buffer_size(buffer);
    uint64_t j = ((uint64_t) i) % (size / sizeof(double));
    fa_buffer_set_double(buffer, j, x);
    return x;
}
fa_signal_t fa_signal_record(fa_buffer_t buffer, fa_signal_t i, fa_signal_t x)
{
    return fa_signal_lift2(fa_string("record"), _record, buffer, i, x);
}

static bool play_started = false; // TODO debug
inline static double _play_stream(fa_ptr_t buffer, double _)
{
    double fa_atomic_ring_buffer_filled(fa_atomic_ring_buffer_t buffer);

    // Unmodified if underflowing
    // TODO report
    double x = 0;

    // if (play_started || (fa_atomic_ring_buffer_filled(buffer) > 1)) {
    {
        play_started = true;
        bool res = fa_atomic_ring_buffer_read_double(buffer, &x);

        if (!res) {
            // fa_warn(fa_string("U"));
        }

        fa_mark_used(res);
    }

    return x;
}

fa_signal_t fa_signal_play_stream(fa_atomic_ring_buffer_t buffer)
{
    return fa_signal_lift(fa_string("playStream"), _play_stream, buffer, fa_signal_constant(0));
}

fa_signal_t fa_signal_record_stream(fa_atomic_ring_buffer_t buffer, fa_signal_t x)
{
    assert(false);
}


struct rec_external {
    fa_string_t name;
    fa_atomic_ring_buffer_t buffer;
};

static fa_ptr_t record_external_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}

static fa_ptr_t record_external_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}

static fa_ptr_t record_external_render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    struct rec_external *ext = (struct rec_external *) x;

    if (!kVectorMode) {
        double x = state->buffer[(offset + 0) * kMaxVectorSize];

        if (ext->buffer) {
            fa_atomic_ring_buffer_write_double(ext->buffer, x);
        }
    } else {
        for (int sample = 0; sample < count; ++sample) {
            double x = state->buffer[(offset + 0) * kMaxVectorSize + sample];

            if (ext->buffer) {
                fa_atomic_ring_buffer_write_double(ext->buffer, x);
            }
        }
    }

    return x;
}

static fa_ptr_t record_external_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    struct rec_external *ext = (struct rec_external *) x;

    if (fa_equal(ext->name, n)) {
        if (ext->buffer) {
            // fa_warn(fa_string_format_integral("Bytes written: %zu", ext->bytes_written));
            fa_atomic_ring_buffer_close(ext->buffer);
        }

        if (!msg || (fa_dynamic_get_type(msg) == atomic_ring_buffer_type_repr)) {
            ext->buffer = msg;
        } else {
            assert(false && "Strange value sent to record_ext");
        }
        // ext->bytes_written = 0;
    }

    return x;
}


fa_signal_t fa_signal_record_external(fa_string_t name,
                                      fa_signal_t signal)
{
    struct rec_external *ext = fa_new_struct(rec_external);
    ext->name = fa_copy(name);
    ext->buffer = NULL;

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = record_external_before_;
    proc->after   = record_external_after_;
    proc->render  = record_external_render_;
    proc->receive = record_external_receive_;
    proc->send    = NULL;
    proc->destroy = NULL;
    proc->data    = ext;

    fa_signal_t s = fa_signal_custom(proc, fa_signal_output_with_custom(proc, 0, 0, signal));
    return s;
}






inline static double _add(fa_ptr_t _, double x, double y)
{
    return x + y;
}
fa_signal_t fa_signal_add(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(fa_string("(+)"), _add, NULL, a, b);
}

inline static double _mul(fa_ptr_t _, double x, double y)
{
    return x * y;
}
fa_signal_t fa_signal_multiply(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(fa_string("(*)"), _mul, NULL, a, b);
}

inline static double _subtract(fa_ptr_t _, double x, double y)
{
    return x - y;
}
fa_signal_t fa_signal_subtract(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(fa_string("(-)"), _subtract, NULL, a, b);
}

inline static double _divide(fa_ptr_t _, double x, double y)
{
    return x / y;
}
fa_signal_t fa_signal_divide(fa_signal_t a, fa_signal_t b)
{
    return fa_signal_lift2(fa_string("(/)"), _divide, NULL, a, b);
}


inline static double _absolute(fa_ptr_t _, double x)
{
    return fabs(x);
}
fa_signal_t fa_signal_absolute(fa_signal_t a)
{
    return fa_signal_lift(fa_string("absolute"), _absolute, NULL, a);
}

inline static double _sin(fa_ptr_t _, double x)
{
    return sin(x);
}
fa_signal_t fa_signal_sin(fa_signal_t a)
{
    return fa_signal_lift(fa_string("sin"), _sin, NULL, a);
}

inline static double _cos(fa_ptr_t _, double x)
{
    return cos(x);
}
fa_signal_t fa_signal_cos(fa_signal_t a)
{
    return fa_signal_lift(fa_string("cos"), _cos, NULL, a);
}






fa_signal_t fa_signal_power(fa_signal_t x, fa_signal_t y)
{
    assert(false && "Not implemented");
}


inline static double _fmod(fa_ptr_t _, double x, double y)
{
    return fmod(x, y);
}
fa_signal_t fa_signal_modulo(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(%)"), _fmod, NULL, x, y);
}

inline static double _not(fa_ptr_t _, double x)
{
    return !x;
}
fa_signal_t fa_signal_not(fa_signal_t x)
{
    return fa_signal_lift(fa_string("not"), _not, NULL, x);
}

inline static double _and(fa_ptr_t _, double x, double y)
{
    return x && y;
}
fa_signal_t fa_signal_and(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(&&)"), _and, NULL, x, y);
}

inline static double _or(fa_ptr_t _, double x, double y)
{
    return x && y;
}
fa_signal_t fa_signal_or(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(||)"), _or, NULL, x, y);
}

inline static double _xor(fa_ptr_t _, double x, double y)
{
    return (double)((int)x ^ (int)y);
}
fa_signal_t fa_signal_xor(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(^)"), _xor, NULL, x, y);
}

// inline static double _eq(fa_ptr_t _, double x, double y)
// {
//     return x == y;
// }
fa_signal_t fa_signal_equal(fa_signal_t x, fa_signal_t y)
{
    assert(false && "Not implemented");
}

inline static double _lt(fa_ptr_t _, double x, double y)
{
    return x < y;
}
fa_signal_t fa_signal_less_than(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(<)"), _lt, NULL, x, y);
}

inline static double _gt(fa_ptr_t _, double x, double y)
{
    return x > y;
}
fa_signal_t fa_signal_greater_than(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(>)"), _gt, NULL, x, y);
}

inline static double _lte(fa_ptr_t _, double x, double y)
{
    return x <= y;
}
fa_signal_t fa_signal_less_than_equal(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(<=)"), _lte, NULL, x, y);
}

inline static double _gte(fa_ptr_t _, double x, double y)
{
    return x >= y;
}
fa_signal_t fa_signal_greater_than_equal(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("(>=)"), _gte, NULL, x, y);
}

fa_signal_t fa_signal_acos(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.acos");
}

fa_signal_t fa_signal_asin(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.sin");
}

fa_signal_t fa_signal_atan(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.atan");
}

// fa_signal_t fa_signal_cos(fa_signal_t x ) { assert (false && "Not implemented"); }

// fa_signal_t fa_signal_sin(fa_signal_t x ) { assert (false && "Not implemented"); }

fa_signal_t fa_signal_tan(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.tan");
}

fa_signal_t fa_signal_exp(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.exp");
}

fa_signal_t fa_signal_log(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.log");
}

fa_signal_t fa_signal_log10(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.log10");
}

fa_signal_t fa_signal_sqrt(fa_signal_t x)
{
    assert(false && "Not implemented: Signal.sqrt");
}

inline static double _min(fa_ptr_t _, double x, double y)
{
    return x < y ? x : y;
}
fa_signal_t fa_signal_min(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("min"), _min, NULL, x, y);
}

inline static double _max(fa_ptr_t _, double x, double y)
{
    return x >= y ? x : y;
}
fa_signal_t fa_signal_max(fa_signal_t x, fa_signal_t y)
{
    return fa_signal_lift2(fa_string("max"), _max, NULL, x, y);
}

fa_signal_t fa_signal_fmod(fa_signal_t x, fa_signal_t y)
{
    assert(false && "Not implemented");
}

fa_signal_t fa_signal_remainder(fa_signal_t x, fa_signal_t y)
{
    assert(false && "Not implemented");
}

fa_signal_t fa_signal_floor(fa_signal_t x, fa_signal_t y)
{
    assert(false && "Not implemented");
}

fa_signal_t fa_signal_ceil(fa_signal_t x, fa_signal_t y)
{
    assert(false && "Not implemented");
}


#ifndef _WIN32

struct _vst_context {
    fa_string_t name;
    AEffect *plugin;
    float **inputs;
    float **outputs;
};

typedef struct _vst_context vst_context;

// TODO remove
static vst_context *last_vst_plug = NULL;


fa_ptr_t vst_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    setPluginParams(plugin, state->rate, kMaxVectorSize);
    resumePlugin(plugin);

    return x;
}
fa_ptr_t vst_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    suspendPlugin(plugin);
    return x;
}

fa_ptr_t vst_render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    // assert(count == 64); // TODO (also change VST loader)

    if (kVectorMode) {
        fa_fail(fa_string("Vector mode not supported!"));
        exit(-1);
    } else {
        // fa_fail(fa_string("Non-Vector mode not supported!"));

        assert(context->inputs && context->outputs);
        silenceChannel(context->inputs, plugin->numInputs, count);
        silenceChannel(context->outputs, plugin->numOutputs, count);

        // TODO inputs

        processAudio(plugin, context->inputs, context->outputs, 1);

        for (int channel = 0; channel < plugin->numOutputs; ++channel) {
            for (int sample = 0; sample < 1; ++sample) {
                if (channel < 2) {
                    state->buffer[(offset + channel)*kMaxVectorSize + sample] = context->outputs[channel][sample];
                }
            }
        }
    }

    return x;
}


// TODO move
#define fa_dynamic_is_fa_string(x) (fa_dynamic_get_type(x) == string_type_repr)
#define fa_dynamic_is_bool(x)   (fa_dynamic_get_type(x) == bool_type_repr)
#define fa_dynamic_is_pair(x)   (fa_dynamic_get_type(x) == pair_type_repr)
#define fa_dynamic_is_list(x)   (fa_dynamic_get_type(x) == list_type_repr)

// Used by vst.cc
void vst_log(const char *msg)
{
    // fa_warn(fa_string_dappend(fa_string("Error in VST: "), fa_string((char*) msg)));
}

void vst_log_i(const char *fmt, long n)
{
    // fa_warn(fa_string_dappend(fa_string("Error in VST: "),
    // fa_string_format_integral((char*) fmt, n)
    // ));
}

fa_ptr_t vst_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    vst_context *context = x;
    AEffect     *plugin = context->plugin;

    // fa_warn(n);
    // fa_warn(context->name);
    // fa_warn(fa_equal(n, context->name) ? fa_string("t") : fa_string("f"));
    // fa_warn(fa_string(""));

    if (fa_equal(n, context->name)) {

        if (fa_dynamic_is_pair(msg) && fa_dynamic_is_fa_string(fa_pair_first(msg)) && fa_equal(fa_pair_first(msg), fa_string("open"))) {
            fa_warn(fa_string("Opening VST window..."));

            if (fa_pair_second(msg)) {
                openPlugin(plugin, fa_pair_second(msg));
            } else {
                fa_warn(fa_string("Asked to open VST window but handle is NULL, ignoring."));
            }

            return x;
        }

        // Assume this is MIDI message
        // TODO add MIDI messags to dynamic and check this!
        if (!fa_midi_message_is_simple(msg)) {
            fa_warn(fa_string("Unknown message to VST plug"));
            return x;
        } else {
            uint8_t status, data1, data2;
            fa_midi_message_decons(msg, &status, &data1, &data2);

            VstMidiEvent event;
            event.type = kVstMidiType;
            event.byteSize = sizeof(VstMidiEvent);
            event.deltaFrames = 0;
            event.flags = 0;
            event.noteLength = 100;
            event.noteOffset = 0;
            // event.midiData[0] = 0x90;
            event.midiData[0] = status;
            event.midiData[1] = data1;
            event.midiData[2] = data2;
            event.midiData[3] = 0;

            event.detune = 0;
            event.noteOffVelocity = 0;

            VstEvents events;
            events.numEvents = 1;
            events.events[0] = (VstEvent *) &event;

            processMidi(plugin, &events);
            return x;
        }

        assert(false && "Unreachable");
    }

    return x;
}

fa_list_t fa_signal_vst(fa_string_t name1, fa_string_t path1, fa_list_t inputs)
{
    // TODO
    fa_string_t name = fa_copy(name1);
    fa_string_t path = fa_copy(path1);

    char *rpath = fa_unstring(path);
    AEffect *plugin = loadPlugin(rpath);
	if (!plugin) {
		fa_warn(concat(fa_string("Could not load plugin "), path));
	}
    initPlugin(plugin);

    // TODO
    assert(canPluginDo(plugin, "receiveVstMidiEvent"));

    // {
    //     WindowRef theWindow;
    //     Rect contentRect;
    //     contentRect.top = 200;
    //     contentRect.left = 300;
    //     contentRect.bottom = 400;
    //     contentRect.right = 500;
    //
    //     // SetRect(&contentRect, 100,100,100,100);
    //     OSStatus err;
    //
    //     err=CreateNewWindow(kDocumentWindowClass,kWindowStandardDocumentAttributes,&contentRect, &theWindow);
    //     if(err!=noErr)printf("Error in CreateNewWindow\n");
    //     ShowWindow(theWindow);
    //
    //     WindowRef refWindow = theWindow;
    //     openPlugin(plugin, refWindow);
    // }


    vst_context *context = fa_malloc(sizeof(vst_context));
    context->plugin = plugin;
    context->name = name;
    fa_warn(fa_string_append(fa_string("Creating VST plugin\n    Name: "), name));
    fa_warn(fa_string_append(fa_string("    Path: "), path));

    context->inputs = malloc(sizeof(fa_ptr_t) * plugin->numInputs);
    context->outputs = malloc(sizeof(fa_ptr_t) * plugin->numOutputs);

    for (int i = 0; i < plugin->numInputs; ++i) {
        context->inputs[i] = malloc(sizeof(float) * kMaxVectorSize);
    }

    for (int i = 0; i < plugin->numOutputs; ++i) {
        context->outputs[i] = malloc(sizeof(float) * kMaxVectorSize);
    }

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = vst_before_;
    proc->after   = vst_after_;
    proc->render  = vst_render_;
    proc->receive = vst_receive_;
    proc->send    = NULL;
    proc->destroy = NULL; // TODO
    proc->data    = context;

    // TODO
    last_vst_plug = context;

    return list(fa_signal_custom(proc,
                                 fa_signal_input_with_custom(proc, 0)),
                fa_signal_input_with_custom(proc, 1));
}

void fa_signal_show_vst_gui(fa_string_t string, void *handle)
{
    // TODO find correct plugin
    openPlugin(last_vst_plug->plugin, handle);
}
#else // _WIN32
fa_list_t fa_signal_vst(fa_string_t name1, fa_string_t path1, fa_list_t inputs)
{
    fa_fail(fa_string("VST not supported for this platform yet."));
}
#endif // _WIN32








/*

// FIXME change this!!!
#define kRecord2Offset 33

struct _record2_context {
    fa_string_t name;
    buffer_t buffer;
};

typedef struct _record2_context record2_context;

fa_ptr_t record2_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
fa_ptr_t record2_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
fa_ptr_t record2_render_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    record2_context *context = x;

    if (!kVectorMode) {
        double x input = state->buffer[(kRecord2Offset + 0)*kMaxVectorSize + 0];
        context->record2 = context->normal;
    } else {
        assert(false && "Not supported yet");
    }

    return x;
}
fa_ptr_t record2_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    record2_context *context = x;

    if (fa_equal(n, context->name)) {
        // TODO accept new buffer
    }

    return x;
}

fa_signal_t fa_signal_record2(fa_string_t name, buffer_t buffer, signal_t i, signal_t x)
{
    record2_context *context = fa_malloc(sizeof(record2_context));
    context->name = name;
    context->buffer = buffer;
    context->i;
    context->x;

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = record2_before_;
    proc->after   = record2_after_;
    proc->render  = record2_render_;
    proc->receive = record2_receive_;
    proc->send    = NULL;
    proc->destroy = NULL; // TODO
    proc->data    = context;

    return fa_signal_custom(proc, fa_signal_input(kRecord2Offset));
}
*/







struct _trigger_context {
    fa_string_t name;
    double normal;
    double trigger;
};

typedef struct _trigger_context trigger_context;

fa_ptr_t trigger_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
fa_ptr_t trigger_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
fa_ptr_t trigger_render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    trigger_context *context = x;

    if (!kVectorMode) {
        state->buffer[(offset + 0)*kMaxVectorSize + 0] = context->trigger;
        context->trigger = context->normal;
    } else {
        assert(false && "Not supported yet");
    }

    return x;
}
fa_ptr_t trigger_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    trigger_context *context = x;

    if (fa_equal(n, context->name)) {
        // Ignore value
        context->trigger = fa_peek_double(msg);
    }

    return x;
}

fa_signal_t fa_signal_trigger(fa_string_t name, double init)
{
    trigger_context *context = fa_malloc(sizeof(trigger_context));
    context->name = name;
    context->normal  = init; // TODO alternate
    context->trigger = context->normal;

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = trigger_before_;
    proc->after   = trigger_after_;
    proc->render  = trigger_render_;
    proc->receive = trigger_receive_;
    proc->send    = NULL;
    proc->destroy = NULL; // TODO
    proc->data    = context;

    return fa_signal_custom(proc, fa_signal_input_with_custom(proc, 0));
}

// --------------------------------------------------------------------------------

struct _play_buffer_slot {
    fa_buffer_t buffer;
    fa_file_buffer_t file_buffer;
    double pos;
    double speed;
    size_t max_pos;
    uint8_t channels;
    fa_sample_type_t sample_type;
    uint8_t sample_size;
    bool playing;
    double volume;        // Set volume
    double pan;           // 
    double left_volume;   // Volume for left channel, calculated from volume and pan
    double right_volume;  // Volume for right channel, calculated from volume and pan
};

typedef struct _play_buffer_slot play_buffer_slot;

struct _play_buffers_context {
    fa_string_t name;
    fa_string_t msg_play;
    fa_string_t msg_stop;
    fa_string_t msg_free;
    fa_string_t msg_volume;
    fa_string_t msg_pan;
    double stream_sample_rate; // cached value
    int slot_count;
    play_buffer_slot *slots;
};

typedef struct _play_buffers_context play_buffers_context;

fa_ptr_t play_buffers_before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    play_buffers_context *context = x;
    context->stream_sample_rate = state->rate;
    return x;
}
fa_ptr_t play_buffers_after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    play_buffers_context *context = x;
    for (int s = 0; s < context->slot_count; s++) {
        play_buffer_slot *slot = &context->slots[s];
        if (slot->buffer) {
            fa_release_reference(slot->buffer);
            slot->buffer = NULL;
        }
        if (slot->file_buffer) {
            fa_release_reference(slot->file_buffer);
            slot->file_buffer = NULL;
        }
    }
    return x;
}
fa_ptr_t play_buffers_render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    play_buffers_context *context = x;

    if (!kVectorMode) {
        
        double summed_left = 0.0;
        double summed_right = 0.0;
        
        for (int s = 0; s < context->slot_count; s++) {
            
            play_buffer_slot *slot = &context->slots[s];
            
            if (slot->playing) {
    
                //slot->pos = state->count % 4410; // TEST!
                //slot->pos =  % 4410; // TEST!
                
                if (slot->file_buffer) {
                    // TODO: interpolating samples for non-integer positions would improve sound quality
                    size_t buffer_pos = ((size_t)slot->pos) * slot->channels;
                    double left, right;
                    switch (slot->sample_type) {
                        case float_sample_type:
                        {
                            left = (double) fa_file_buffer_get_float(slot->file_buffer, buffer_pos + 0);
                            if (slot->channels > 1) {
                                right = (double) fa_file_buffer_get_float(slot->file_buffer, buffer_pos + 1);
                            } else {
                                right = left;
                            }
                            break;
                        }
                        case double_sample_type:
                        {
                            left = fa_file_buffer_get_double(slot->file_buffer, buffer_pos + 0);
                            if (slot->channels > 1) {
                                right = fa_file_buffer_get_double(slot->file_buffer, buffer_pos + 1);
                            } else {
                                right = left;
                            }
                            break;
                        }
                    }
                    summed_left  += left * slot->left_volume;
                    summed_right += right * slot->right_volume;
                    slot->pos += slot->speed; // TODO: is double precision good enough not to get drifting errors?
                    
                    if (state->count % 11025 == 0) { // TODO: how do we know that this is often enough?
                        fa_file_buffer_hint(slot->file_buffer, buffer_pos * slot->sample_size);
                    }
                }
    
                else if (slot->pos < slot->max_pos) {
                    // TODO: interpolating samples for non-integer positions would improve sound quality
                    size_t buffer_pos = ((size_t)slot->pos) * slot->channels;
                    double left, right;
                    // Left channel
                    left = fa_buffer_get_double(slot->buffer, buffer_pos + 0);
                    // Right channel: if stereo, get the right channel sample, otherwise, copy the left channel sample
                    if (slot->channels > 1) {
                        right = fa_buffer_get_double(slot->buffer, buffer_pos + 1);
                    } else {
                        right = left;
                    }
                    summed_left  += left * slot->left_volume;
                    summed_right += right * slot->right_volume;
                    slot->pos += slot->speed; // TODO: is double precision good enough not to get drifting errors?
                } else {
                    slot->playing = false;
                    slot->pos = 0;
                }
            }   
        }
        
        state->buffer[(offset + 0)*kMaxVectorSize] = summed_left;
        state->buffer[(offset + 1)*kMaxVectorSize] = summed_right;
        
    } else {
        assert(false && "Vector mode not supported yet");
    }

    return x;
}

#define piBy4      0.78539816339
#define kSqrt2by2  0.70710678118

static void update_slot_volume(play_buffer_slot *slot)
{
    double angle = slot->pan * piBy4;
    slot->left_volume = slot->volume * kSqrt2by2 * (cos(angle) - sin(angle));
    slot->right_volume = slot->volume * kSqrt2by2 * (cos(angle) + sin(angle));
}

fa_ptr_t play_buffers_receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    play_buffers_context *context = x;

    if (fa_equal(n, context->name)) {
        
        // fa_slog_info("play_buffers_receive_ ", msg);

        // Accept single commands, and make slot default to 0
        int slot_num = 0;
        fa_ptr_t cmd = msg;
        
        // If msg is a pair, assume it is a slot number and a command
        if (fa_dynamic_get_type(msg) == pair_type_repr) {
            slot_num = fa_peek_integer(fa_pair_first(msg));
            cmd = fa_pair_second(msg);
        }
        
        if (slot_num >= 0 && slot_num < context->slot_count) {
            play_buffer_slot *slot = &context->slots[slot_num];
            
            fa_dynamic_type_repr_t cmd_type = fa_dynamic_get_type(cmd);
            switch (cmd_type) {
                case buffer_type_repr:
                {
                    slot->playing = false;
                    slot->pos = 0;
                    fa_buffer_t buffer = cmd;
                    if (buffer != slot->buffer) {
                        fa_take_reference(buffer);
                        if (slot->buffer) {
                            fa_release_reference(slot->buffer);
                        }
                        if (slot->file_buffer) {
                            fa_release_reference(slot->file_buffer);
                        }
                        slot->buffer = buffer;
                        slot->file_buffer = NULL;
                        slot->channels = fa_peek_number(fa_get_meta(buffer, fa_string("channels")));
                        size_t size = fa_buffer_size(buffer);
                        slot->max_pos = (size / ((sizeof(double)) * slot->channels)) - 1;
                        double buffer_rate = fa_peek_number(fa_get_meta(buffer, fa_string("sample-rate")));
                        slot->speed = buffer_rate / context->stream_sample_rate;
                    }
                    break;
                }
                
                case file_buffer_type_repr:
                {
                    slot->playing = false;
                    slot->pos = 0;
                    fa_file_buffer_t buffer = cmd;
                    if (buffer != slot->file_buffer) {
                        fa_take_reference(buffer);
                        if (slot->file_buffer) {
                            fa_release_reference(slot->file_buffer);
                        }
                        if (slot->buffer) {
                            fa_release_reference(slot->buffer);
                        }
                        slot->file_buffer = buffer;
                        slot->buffer = NULL;
                        slot->channels = fa_peek_number(fa_get_meta(buffer, fa_string("channels")));
                        slot->max_pos = fa_peek_integer(fa_get_meta(buffer, fa_string("frames")));
                        double buffer_rate = fa_peek_number(fa_get_meta(buffer, fa_string("sample-rate")));
                        slot->speed = buffer_rate / context->stream_sample_rate;
                        slot->sample_type = fa_peek_integer(fa_get_meta(buffer, fa_string("sample-type")));
                        slot->sample_size = fa_sample_type_size(slot->sample_type);
                    }
                    break;
                }
                
                // A number: a new position in frames
                case i8_type_repr:
                case i16_type_repr:
                case i32_type_repr:
                case i64_type_repr:
                case f32_type_repr:
                case f64_type_repr:
                {
                    double new_pos = fa_peek_number(cmd);
                    if (new_pos > slot->max_pos) new_pos = slot->max_pos;
                    slot->pos = new_pos;
                    if (slot->file_buffer) {
                        fa_file_buffer_hint(slot->file_buffer, slot->pos * slot->channels * slot->sample_size);
                    }
                    // // For testing only, can't do I/O in this thread
                    // if (slot->file_buffer) {
                    //     uint8_t frame_size = slot->channels * slot->sample_size;
                    //     fa_file_buffer_seek_if_needed(slot->file_buffer, slot->pos * frame_size);
                    // }
                    break;
                }
                
                case string_type_repr:
                {
                    if (fa_equal(cmd, context->msg_play)) {
                        slot->playing = true;
                    }
                    else if (fa_equal(cmd, context->msg_stop)) {
                        slot->playing = false;
                    }
                    else if (fa_equal(cmd, context->msg_free)) {
                        if (slot->buffer) {
                            fa_buffer_t buffer = slot->buffer;
                            slot->playing = false;
                            slot->pos = 0;
                            slot->buffer = NULL;
                            fa_release_reference(buffer);
                        }
                        if (slot->file_buffer) {
                            fa_file_buffer_t buffer = slot->file_buffer;
                            slot->playing = false;
                            slot->pos = 0;
                            slot->file_buffer = NULL;
                            fa_release_reference(buffer);
                        }
                    }
                    break;
                }
                
                case pair_type_repr:
                {
                    fa_ptr_t setting = fa_pair_first(cmd);
                    fa_ptr_t value   = fa_pair_second(cmd);
                    if (fa_equal(setting, context->msg_volume)) {
                        double volume = fa_peek_number(value);
                        if (volume < 0) volume = 0;
                        if (volume > 64) volume = 64; // arbitrary max value
                        slot->volume = volume;
                        update_slot_volume(slot);
                    }
                    else if (fa_equal(setting, context->msg_pan)) {
                        double pan = fa_peek_number(value);
                        if (pan < -1.0) pan = -1.0;
                        if (pan > 1.0) pan = 1.0;
                        slot->pan = pan;
                        update_slot_volume(slot);
                    }
                    
                    break;
                }
    
                default:
                break;
            }
        }
    }
    
    return x;
}

fa_ptr_t play_buffers_destroy_(fa_ptr_t x)
{
    play_buffers_context *context = x;
    fa_destroy(context->name);
    fa_destroy(context->msg_play);
    fa_destroy(context->msg_stop);
    fa_destroy(context->msg_free);
    fa_destroy(context->msg_volume);
    fa_destroy(context->msg_pan);
    fa_free(context->slots);
    fa_free(context);
    return NULL;
}

fa_pair_t fa_signal_play_buffers(fa_string_t name, int count)
{
    assert(count > 0 && count <= 32);
    play_buffers_context *context = fa_malloc(sizeof(play_buffers_context));
    context->name = fa_copy(name);
    context->stream_sample_rate = 0;
    
    // Cache messages, so that we don't have to allocate in the receive function
    context->msg_play    = fa_string("play");
    context->msg_stop    = fa_string("stop");
    context->msg_free    = fa_string("free");
    context->msg_volume  = fa_string("volume");
    context->msg_pan     = fa_string("pan");
    
    // Init slots
    context->slots = fa_malloc(count * sizeof(play_buffer_slot));
    context->slot_count = count;
    for (int s = 0; s < count; s++) {
        play_buffer_slot *slot = &context->slots[s];
        slot->buffer = NULL;
        slot->file_buffer = NULL;
        slot->pos = 0;
        slot->max_pos = 0;
        slot->playing = false;
        slot->speed = 1.0;
        slot->sample_type = double_sample_type;
        slot->sample_size = fa_sample_type_size(double_sample_type);
        slot->volume = 1.0;
        slot->pan = 0.0;
        slot->left_volume = 1.0;
        slot->right_volume = 1.0;
    }

    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = play_buffers_before_;
    proc->after   = play_buffers_after_;
    proc->render  = play_buffers_render_;
    proc->receive = play_buffers_receive_;
    proc->send    = NULL;
    proc->destroy = play_buffers_destroy_;
    proc->data    = context;

    fa_signal_t left  = fa_signal_input_with_custom(proc, 0);
    fa_signal_t right = fa_signal_input_with_custom(proc, 1);
    fa_signal_t left2 = fa_signal_custom(proc, left);
    fa_pair_t result = fa_pair_create(left2, right);
    return result;
}

fa_pair_t fa_signal_play_buffer(fa_string_t name)
{
    return fa_signal_play_buffers(name, 1);
}

// --------------------------------------------------------------------------------

fa_ptr_t signal_copy(fa_ptr_t a)
{
    return fa_signal_copy(a);
}

void signal_destroy(fa_ptr_t a)
{
    return fa_signal_destroy(a);
}

fa_ptr_t signal_add(fa_ptr_t a, fa_ptr_t b)
{
    return fa_signal_add(a, b);
}

fa_ptr_t signal_subtract(fa_ptr_t a, fa_ptr_t b)
{
    return fa_signal_subtract(a, b);
}

fa_ptr_t signal_multiply(fa_ptr_t a, fa_ptr_t b)
{
    return fa_signal_multiply(a, b);
}

fa_ptr_t signal_divide(fa_ptr_t a, fa_ptr_t b)
{
    return fa_signal_divide(a, b);
}

fa_ptr_t signal_absolute(fa_ptr_t a)
{
    return fa_signal_absolute(a);
}

fa_string_t signal_show(fa_ptr_t a)
{
	//return fa_string("TREE");
    return fa_signal_draw_tree(fa_signal_to_tree(fa_signal_simplify(a)));
}

fa_ptr_t signal_impl(fa_id_t interface)
{
    static fa_copy_t signal_copy_impl
        = { signal_copy };
    static fa_destroy_t signal_destroy_impl
        = { signal_destroy };
    static fa_number_t  signal_number_impl
        = { signal_add, signal_subtract, signal_multiply, signal_divide, signal_absolute };
    static fa_string_show_t signal_show_impl
        = { signal_show };

    switch (interface) {
    case fa_copy_i:
        return &signal_copy_impl;

    case fa_destroy_i:
        return &signal_destroy_impl;

    case fa_number_i:
        return &signal_number_impl;

    case fa_string_show_i:
        return &signal_show_impl;

    default:
        return NULL;
    }
}

