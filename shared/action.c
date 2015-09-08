
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/action.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/pair/left.h>
#include <fa/func_ref.h>
#include <fa/priority_queue.h>
#include <fa/func_ref.h>
#include <pthread.h>

// Scheduled events are forwarded to the audio thread in advance. This is the
// threshold value in milliseconds. A too low value may cause actions to be
// executed too late, if the scheduler is busy or for some other reason cannot
// forward the actions in time. A higher value minimizes that risk, but actions
// that have been forwarded to the audio thread cannot be cancelled, so a very
// high value will make e.g. stop playback appear sluggish. Also, action_do
// (and action_do_with_time) is executed in the audio control thread rather
// than the audio thread, which means that they will be executed (more) too early.
#define kScheduleLookahead 80

// Actions 
#define kScheduleMaxAge    5000


typedef fa_action_t                 action_t;               // 
typedef fa_action_channel_t         channel_t;              // int
typedef fa_action_name_t            name_t;                 // fa_string_t
typedef fa_signal_unary_double_t    unary_double_t;         // unwrapped function reference
typedef fa_action_nullary_with_time_t nullary_with_time_t;  // unwrapped function reference

struct _fa_action_t {

    fa_impl_t                       impl;
    
    enum {
        get_action,
        set_action,
        accum_action,
        send_action,
        do_action,
        compound_action
    }                               tag;

    union {
        struct {
            channel_t               channel;
            unary_double_t          function;
            fa_ptr_t                data;
        }                           get;

        struct {
            channel_t               channel;
            double                  value;
        }                           set;

        struct {
            channel_t               channel;
            unary_double_t          function;
            fa_ptr_t                data;
        }                           accum;

        struct {
            name_t                  name;
            fa_ptr_t                value;
            bool                    retain;
        }                           send;

        struct {
            fa_nullary_t            function;
            nullary_with_time_t     function_with_time; // Either function may be set, data is the same
            fa_ptr_t                data;
        }                           do_;

        struct {
            // Action -> NULL or (SimpleAction, (Time, Action))
            fa_unary_t              function;
            fa_ptr_t                data;
        }                           compound;
        
    }                               fields;
    
    //uint64_t timestamp; // Eriks test
    double timestamp; // Eriks test
};

//static fa_map_t all_actions = NULL;
//static fa_list_t all_actions = NULL;
static int gActionCount = 0;

inline static action_t new_action(int tag)
{
    fa_ptr_t action_impl(fa_id_t interface);

    action_t s = fa_new(action);
    s->impl = &action_impl;
    //s->ref_count = 1;
    s->tag  = tag;
    
    // if (!all_actions) {
//         all_actions = fa_list_empty();
//     }
//     all_actions = fa_list_dcons(s, all_actions);
    
    gActionCount++;
    
    return s;
}

inline static void delete_action(action_t action)
{
    gActionCount--;
    fa_delete(action);
}

#define is_get(v)           (v->tag == get_action)
#define is_set(v)           (v->tag == set_action)
#define is_accum(v)         (v->tag == accum_action)
#define is_send(v)          (v->tag == send_action)
#define is_do(v)            (v->tag == do_action)
#define is_compound(v)      (v->tag == compound_action)

#define get_get(v,f)        v->fields.get.f
#define set_get(v,f)        v->fields.set.f
#define accum_get(v,f)      v->fields.accum.f
#define send_get(v,f)       v->fields.send.f
#define do_get(v,f)         v->fields.do_.f
#define compound_get(v,f)   v->fields.compound.f

// --------------------------------------------------------------------------------

fa_action_t fa_action_get(fa_action_channel_t channel,
                          unary_double_t function,
                          fa_ptr_t data)
{
    action_t action = new_action(get_action);
    get_get(action, channel)    = channel;
    get_get(action, function)   = function;
    get_get(action, data)       = data;
    return action;
}

fa_action_t fa_action_set(channel_t channel, double value)
{
    action_t action = new_action(set_action);
    set_get(action, channel) = channel;
    set_get(action, value)   = value;
    return action;
}

fa_action_t fa_action_accum(channel_t       channel,
                            unary_double_t  function,
                            fa_ptr_t        data)
{
    action_t action = new_action(accum_action);
    accum_get(action, channel)  = channel;
    accum_get(action, function) = function;
    accum_get(action, data)     = data;
    return action;
}


fa_action_t fa_action_send(fa_action_name_t name, fa_ptr_t value)
{
    assert(name && "NULL name sent to fa_action_send");
#ifdef FAUDIO_DEBUG
    fa_dynamic_type_repr_t type = fa_dynamic_get_type(value);
    if (type == buffer_type_repr || type == atomic_ring_buffer_type_repr) {
        fa_warn(fa_string("Sending buffer or ring buffer as an action_send, should probable use action_send_retain instead"));
    }
#endif
    action_t action = new_action(send_action);
    send_get(action, name)  = fa_copy(name);
    send_get(action, value) = value;
    send_get(action, retain) = false;
    return action;
}

fa_action_t fa_action_send_retain(fa_action_name_t name, fa_action_value_t value)
{
    action_t action = new_action(send_action);
    send_get(action, name)  = fa_copy(name);
    send_get(action, value) = value;
    send_get(action, retain) = true;
    return action;
}

fa_action_t fa_action_do(fa_nullary_t function, fa_ptr_t data)
{
    action_t action = new_action(do_action);
    do_get(action, function)            = function;
    do_get(action, function_with_time)  = NULL;
    do_get(action, data)                = data;
    return action;
}

fa_action_t fa_action_do_with_time(nullary_with_time_t function_with_time, fa_ptr_t data)
{
    action_t action = new_action(do_action);
    do_get(action, function)            = NULL;
    do_get(action, function_with_time)  = function_with_time;
    do_get(action, data)                = data;
    return action;
}

fa_action_t fa_action_compound(fa_unary_t function, fa_ptr_t data)
{
    action_t action = new_action(compound_action);
    compound_get(action, function)  = function;
    compound_get(action, data)      = data;
    return action;
}

static inline fa_action_t copy_get(fa_action_t action2)
{
    action_t action = new_action(get_action);
    get_get(action, channel)    = get_get(action2, channel);
    get_get(action, function)   = get_get(action2, function);
    get_get(action, data)       = get_get(action2, data);
    return action;
}
static inline fa_action_t copy_set(fa_action_t action2)
{
    action_t action = new_action(set_action);
    set_get(action, channel) = set_get(action2, channel);
    set_get(action, value)   = set_get(action2, value);
    return action;
}
static inline fa_action_t copy_accum(fa_action_t action2)
{
    action_t action = new_action(accum_action);
    accum_get(action, channel)  = accum_get(action2, channel);
    accum_get(action, function) = accum_get(action2, function);
    accum_get(action, data)     = accum_get(action2, data);
    return action;
}
static inline fa_action_t copy_send(fa_action_t action2)
{
    action_t action = new_action(send_action);
    send_get(action, name)   = fa_copy(send_get(action2, name)); // the name string is owned by the action
    if (send_get(action2, retain)) {
        send_get(action, value) = send_get(action2, value);
    } else {
        send_get(action, value) = fa_copy(send_get(action2, value));
    }
    send_get(action, retain) = send_get(action2, retain);
    return action;
}
static inline fa_action_t copy_do(fa_action_t action2)
{
    action_t action = new_action(do_action);
    do_get(action, function)            = do_get(action2, function);
    do_get(action, function_with_time)  = do_get(action2, function_with_time);
    do_get(action, data)                = do_get(action2, data);
    return action;
}
static inline fa_action_t copy_compound(fa_action_t action2)
{
    action_t action = new_action(compound_action);
    compound_get(action, function) = compound_get(action2, function);
    if (compound_get(action2, data)) {
        compound_get(action, data) = fa_deep_copy(compound_get(action2, data));
    } else {
        compound_get(action, data) = NULL; // Important! new_action does not allocate nulled memory
    }
    return action;
}

/*
    For set, no copy needed
    For send, the scheduled value is copied using fa_copy
    For predicate/do, the closure is *not* copied
    For compound actions, we deep copy the entire structure of the data field

    NOTE: there is no difference between copying and deep-copying an action!
*/
fa_action_t fa_action_copy(fa_action_t action)
{
    switch (action->tag) {
    case get_action:
        return copy_get(action);

    case set_action:
        return copy_set(action);

    case accum_action:
        return copy_accum(action);

    case send_action:
        return copy_send(action);

    case do_action:
        return copy_do(action);

    case compound_action:
        return copy_compound(action);

    default:
        assert(false);
    }
}

// static inline fa_action_t deep_copy_send(fa_action_t action2)
// {
//     action_t action = new_action(send_action);
//     send_get(action, name)  = fa_deep_copy(send_get(action2, name));
//     send_get(action, value) = fa_deep_copy(send_get(action2, value));
//     return action;
// }
// static inline fa_action_t deep_copy_compound(fa_action_t action2)
// {
//     action_t action = new_action(compound_action);
//     compound_get(action, function)  = compound_get(action2, function);
//     compound_get(action, data)      = fa_deep_copy(compound_get(action2, data));
//     return action;
// }


// /*
//     For set, no copy needed
//     TODO: what about get/accum? Should the data pointer be copied?
//     For send, the scheduled value is copied using fa_copy
//     TODO: For predicate/do, the closure is *not* copied
// */
//
// fa_action_t fa_action_deep_copy(fa_action_t action)
// {
//     switch (action->tag) {
//     case get_action:
//         return copy_get(action);  // no deep copying needed
//
//     case set_action:
//         return copy_set(action); // no deep copying needed (?)
//
//     case accum_action:
//         return copy_accum(action); // no deep copying needed (?)
//
//     case send_action:
//         return deep_copy_send(action);
//
//     case do_action:
//         return copy_do(action); // no deep copying needed? What about the closure?
//
//     case compound_action:
//         return deep_copy_compound(action);
//
//     default:
//         assert(false);
//     }
// }
//
//


static inline void destroy_get(fa_action_t action)
{
    fa_mark_used(action);
}
static inline void destroy_set(fa_action_t action)
{
	// Nothing to free, only int and double used
    fa_mark_used(action);
}
static inline void destroy_accum(fa_action_t action)
{
    fa_mark_used(action);
}
static inline void destroy_send(fa_action_t action)
{
    fa_destroy(send_get(action, name));
    if (send_get(action, value) && !send_get(action, retain)) {
        fa_destroy(send_get(action, value));
    }
}
static inline void destroy_do(fa_action_t action)
{
    fa_mark_used(action);
}

// static void deep_destroy_unless_action(fa_ptr_t x);
//
// static void deep_destroy_unless_action(fa_ptr_t x) {
//     assert(x && "NULL passed to destroy_unless_action");
//     switch (fa_dynamic_get_type(x)) {
//     case list_type_repr:
//         fa_for_each(e, x) {
//             deep_destroy_unless_action(e);
//         }
//         fa_destroy(x);
//         break;
//     case pair_type_repr: {
//         fa_ptr_t a = fa_pair_first(x);
//         fa_ptr_t b = fa_pair_second(x);
//         if (a) deep_destroy_unless_action(a);
//         if (b) deep_destroy_unless_action(b);
//         fa_destroy(x);
//         break; }
//     case action_type_repr:
//         // Do nothing
//         break;
//     default:
//         fa_destroy(x);
//     }
// }

// static bool is_not_action(fa_ptr_t x) {
//     bool b;
//     if (!x) {
//         b = false;
//     } else {
//         b = (!fa_dynamic_check(x)) || (fa_dynamic_get_type(x) != action_type_repr);
//         if (b) fa_slog_info("    > destroying ", x);
//         else fa_slog_info("    > skipping ", x);
//     }
//     return b;
// }

static fa_ptr_t _repeat(fa_ptr_t data, fa_ptr_t compound);
static fa_ptr_t _if(fa_ptr_t data, fa_ptr_t compound);

static inline void destroy_compound(fa_action_t action)
{
    assert(action && "No action in destroy_compound");
    
    //fa_slog_info("destroy_compound: ", action);
    //fa_slog_info("destroy_compound  in thread:   ", fa_string_format_integral("%p", (long) pthread_self()));
    
    //fa_slog_info("destroy_compound ", action);
    fa_ptr_t data = compound_get(action, data);
    if (data) {
        fa_destroy(data); // TODO: is this the right depth? cannot use deep_destroy here!
    }
}

void fa_action_destroy(fa_action_t action)
{
    //fa_slog_info("fa_action_destroy ", action);
    //fa_slog_info("destroy_action    in thread:   ", fa_string_format_integral("%p", (long) pthread_self()));
    switch (action->tag) {
    case get_action:
        destroy_get(action);
		break;

    case set_action:
        destroy_set(action);
		break;

    case accum_action:
        destroy_accum(action);
		break;

    case send_action:
        destroy_send(action);
		break;

    case do_action:
        destroy_do(action);
		break;

    case compound_action:
        destroy_compound(action);
		break;

    default:
        assert(false && "unknown action type in fa_action_destroy");
    }

    //action->ref_count -= 100;
    delete_action(action);
}


static inline void deep_destroy_send(fa_action_t action, fa_deep_destroy_pred_t pred)
{
    fa_deep_destroy(send_get(action, name), pred);
    if (send_get(action, value) && !send_get(action, retain)) {
        fa_deep_destroy(send_get(action, value), pred);
    }
}
static inline void deep_destroy_compound(fa_action_t action, fa_deep_destroy_pred_t pred)
{
	fa_ptr_t data = compound_get(action, data);
    if (data) fa_deep_destroy(data, pred);
}

void fa_action_deep_destroy(fa_action_t action, fa_deep_destroy_pred_t pred)
{
    if (!(pred(action))) return;
    // fa_slog_info("fa_action_deep_destroy ", action);
    switch (action->tag) {
    case get_action:
        destroy_get(action);
		break;

    case set_action:
        destroy_set(action);
		break;

    case accum_action:
        destroy_accum(action);
		break;

    case send_action:
        deep_destroy_send(action, pred);
		break;

    case do_action:
        destroy_do(action);
		break;

    case compound_action:
        deep_destroy_compound(action, pred);
		break;

    default:
        assert(false && "unknown action type in fa_action_deep_destroy");
    }

    //action->ref_count -= 1000;
    delete_action(action);
}

double fa_action_timestamp(fa_action_t action) {
    return action->timestamp;
}

void fa_action_timestamp_set(fa_action_t action, double timestamp) {
    action->timestamp = timestamp;
}

bool fa_action_is_get(fa_action_t action)
{
    return is_get(action);
}

fa_action_channel_t fa_action_get_channel(fa_action_t action)
{
    assert(is_get(action) && "Not a get action");
    return get_get(action, channel);
}

fa_signal_unary_double_t fa_action_get_function(fa_action_t action)
{
    assert(is_get(action) && "Not a get action");
    return get_get(action, function);
}

fa_ptr_t fa_action_get_data(fa_action_t action)
{
    assert(is_get(action) && "Not a get action");
    return get_get(action, data);
}


bool fa_action_is_set(fa_action_t action)
{
    return is_set(action);
}

fa_action_channel_t fa_action_set_channel(fa_action_t action)
{
    assert(is_set(action) && "Not a set action");
    return set_get(action, channel);
}

double fa_action_set_value(fa_action_t action)
{
    assert(is_set(action) && "Not a set action");
    return set_get(action, value);
}



bool fa_action_is_accum(fa_action_t action)
{
    return is_accum(action);
}

fa_action_channel_t fa_action_accum_channel(fa_action_t action)
{
    assert(is_accum(action) && "Not an accum action");
    return accum_get(action, channel);
}

fa_signal_unary_double_t fa_action_accum_function(fa_action_t action)
{
    assert(is_accum(action) && "Not an accum action");
    return accum_get(action, function);
}

fa_ptr_t fa_action_accum_data(fa_action_t action)
{
    assert(is_accum(action) && "Not an accum action");
    return accum_get(action, data);
}



bool fa_action_is_send(fa_action_t action)
{
    return is_send(action);
}

fa_action_name_t fa_action_send_name(fa_action_t action)
{
    assert(is_send(action) && "Not a send action");
    return send_get(action, name);
}

fa_action_value_t fa_action_send_value(fa_action_t action)
{
    assert(is_send(action) && "Not a send action");
    return send_get(action, value);
}

bool fa_action_is_simple(fa_action_t action)
{
    return !is_compound(action);
}

bool fa_action_is_compound(fa_action_t action)
{
    return is_compound(action);
}


bool fa_action_is_do(fa_action_t action)
{
    return is_do(action);
}


/*void fa_action_compound_decons(fa_action_t action) {
    
}*/

// Render a compound action by calling its function, deconstruct the result and destroy the pairs used
static void compound_render(fa_action_t compound, fa_action_t* first, fa_time_t* interval, fa_action_t* rest)
{
    assert(compound && "NULL action in compound_render");
    assert(is_compound(compound) && "Not a compound action");
    *first = NULL;
    *interval = NULL;
    *rest = NULL;
    // Call the function
    fa_pair_t maybeFirstRest = compound_get(compound, function)(compound_get(compound, data), compound);
    if (maybeFirstRest) {
        *first = fa_pair_first(maybeFirstRest);
        fa_pair_t maybeRest = fa_pair_second(maybeFirstRest);
        if (maybeRest) {
            *interval = fa_pair_first(maybeRest);
            *rest     = fa_pair_second(maybeRest);
            fa_destroy(maybeRest);
        }
        fa_destroy(maybeFirstRest);
    }
}


// Action -> Maybe (Maybe Action, Maybe (Time, Action))
// void compound_render(fa_action_t compound, fa_action_t* action, fa_time_t* interval, fa_action_t* rest)
// fa_pair_t compound_render(fa_action_t action)
// {
//     assert(is_compound(action) && "Not a compound action");
//     return compound_get(action, function)(compound_get(action, data), action);
// }
//
// fa_action_t fa_action_compound_first(fa_action_t action)
// {
//     fa_pair_t maybeFirstRest = compound_render(action);
//
//     if (maybeFirstRest) {
//         return fa_pair_first(maybeFirstRest);
//     } else {
//         return NULL;
//     }
// }
//
// fa_time_t fa_action_compound_interval(fa_action_t action)
// {
//     fa_pair_t maybeFirstRest = compound_render(action);
//
//     if (maybeFirstRest) {
//         fa_pair_t maybeRest = fa_pair_second(maybeFirstRest);
//
//         if (maybeRest) {
//             return fa_pair_first(maybeRest);
//         }
//     }
//
//     return NULL;
// }
//
// fa_action_t fa_action_compound_rest(fa_action_t action)
// {
//     fa_pair_t maybeFirstRest = compound_render(action);
//
//     if (maybeFirstRest) {
//         fa_pair_t maybeRest = fa_pair_second(maybeFirstRest);
//
//         if (maybeRest) {
//             return fa_pair_second(maybeRest);
//         }
//     }
//
//     return NULL;
// }



static inline fa_ptr_t _null(fa_ptr_t data, fa_ptr_t compound)
{
    return NULL;
}

fa_action_t fa_action_null()
{
    return fa_action_compound(_null, NULL);
}



#define unpair(x,y,p) fa_pair_decons((fa_ptr_t*) &x, (fa_ptr_t*) &y, p)

static fa_ptr_t _repeat(fa_ptr_t data, fa_ptr_t c)
{
    fa_action_t compound = c;
    fa_pair_t interval_times;
    action_t action;
    fa_time_t interval;
    fa_ptr_t times;
    unpair(interval_times, action, data);
    unpair(interval, times, interval_times);
    
    //assert(fa_is_int16(times) && fa_peek_int16(times) == 0);
    
    
    //printf("About to copy\n");
    //fa_slog_info("  object: ", compound);
    //fa_slog_info("repeat            in thread:   ", fa_string_format_integral("%p", (long) pthread_self()));
    
    //printf("Copied!\n");
    if (fa_peek_int16(times) == 0) {
        fa_action_t copy = fa_deep_copy(compound);
        fa_destroy(data); // only the pair
        fa_destroy(interval_times);
        compound_get(compound, data) = NULL; // reset reference, to avoid double free
        return fa_pair_create(action, pair(interval, copy));
    } else {
        times = fa_dsubtract(times, fa_from_int16(1));
        if (fa_peek_int16(times) == 0) {
            fa_destroy(data); // only the pair
            fa_destroy(interval_times);
            fa_destroy(interval);
            compound_get(compound, data) = NULL; // reset reference, to avoid double free
            return fa_pair_create(action, NULL);
        } else {
            // TODO: decrement times in the copy
            fa_action_t copy = fa_deep_copy(compound);
            fa_destroy(data); // only the pair
            fa_destroy(interval_times);
            compound_get(compound, data) = NULL; // reset reference, to avoid double free
            return fa_pair_create(action, pair(interval, copy));
        }
    }
}

fa_action_t fa_action_repeat(fa_time_t interval, size_t times, fa_action_t action)
{
    assert(times == 0 && "Not implemented");
    return fa_action_compound(_repeat, pair(pair(interval, fa_from_int16(times)), action));
}



static fa_ptr_t _many(fa_ptr_t data, fa_ptr_t c)
{
    fa_list_t timeActions = data;
    fa_action_t compound = c;

    if (!timeActions || fa_list_is_empty(timeActions)) {
        // fa_slog_info("  IN _many  (list is empty)", compound);
        return NULL;
    } else {
        fa_pair_t first_interval = fa_list_head(timeActions);
        action_t  first          = fa_pair_first(first_interval);
        fa_time_t interval       = fa_pair_second(first_interval);
        
        // Pop first element in list
        fa_destroy(first_interval);
        timeActions = fa_list_dtail(timeActions);
        compound_get(compound, data) = NULL; // reset reference, to avoid double free
        
        // If there is only one action, return it
        if (fa_list_is_empty(timeActions)) {
            fa_destroy(interval);
            fa_destroy(timeActions);
            return fa_pair_create(first, NULL);
        }
        
        // There are more actions, so return them as well for rescheduling
        action_t rest = fa_action_many(timeActions);
        return fa_pair_create(first, fa_pair_create(interval, rest));
    }
}

// [(Action, Time)] -> Action
fa_action_t fa_action_many(fa_list_t timeActions)
{
    // // DEBUG
    // assert(fa_dynamic_get_type(timeActions) == list_type_repr);
    // fa_for_each(x, timeActions) {
    //     assert(fa_dynamic_get_type(x) == pair_type_repr);
    //     assert(fa_dynamic_get_type(fa_pair_first(x)) == action_type_repr);
    //     //assert(fa_dynamic_get_type(fa_pair_second(x)) == time_type_repr);
    // }
    // // END DEBUG
    
    return fa_action_compound(_many, timeActions);
}

static void _flatten(fa_action_t action, fa_list_t *alist) {
    if (fa_action_is_simple(action)) {
        fa_push_list(action, *alist);
    } else {
        if (compound_get(action, function) == _many) {
            assert(fa_dynamic_get_type(compound_get(action, data)) == list_type_repr);
            fa_for_each(x, compound_get(action, data)) {
                _flatten(fa_pair_first(x), alist);
                fa_destroy(fa_pair_second(x)); // the time
                fa_destroy(x);  // the pair
            }
            fa_destroy(action); // including the list
        } else {
            fa_deep_destroy_always(action);
        }
    }
}

fa_list_t fa_action_flat_to_list(fa_action_t action) {
    fa_list_t alist = fa_list_empty();
    _flatten(action, &alist);
    return fa_list_dreverse(alist);
}

bool fa_action_is_flat(fa_action_t action) {
    if (is_do(action)) return false;
    if (fa_action_is_simple(action)) return true;
    if (compound_get(action, function) != _many) return false;
    fa_for_each(x, compound_get(action, data)) {
        if (!fa_action_is_flat(fa_pair_first(x)) || !fa_time_is_zero(fa_pair_second(x))) return false;
    }
    return true;
}

static fa_ptr_t _if(fa_ptr_t data, fa_ptr_t c)
{
    fa_action_t   compound = c;
    fa_pair_t     pred_action = data;
    fa_func_ref_t pred_closure;
    fa_pred_t     pred_function;
    fa_ptr_t      pred_data;
    action_t      action;
    unpair(pred_closure, action, pred_action);
    pred_function = fa_func_ref_func(pred_closure);
    pred_data = fa_func_ref_data(pred_closure);
    fa_destroy(pred_closure);
    compound_get(compound, data) = NULL; // reset reference, to avoid double free

    if (fa_action_is_simple(action)) {
        if (pred_function(pred_data, NULL)) {
            return fa_pair_create(action, NULL);
        } else {
            fa_action_destroy(action);
            return NULL;
        }
    } else {
        action_t  first;
        fa_time_t interval;
        action_t  rest;
        compound_render(action, &first, &interval, &rest);
        
        fa_action_destroy(action);
        
        if (!first && !rest) {
            return NULL;
        }
        
        if (pred_function(pred_data, NULL)) {
            if (rest) {
                fa_action_t new_if = fa_action_if(pred_function, pred_data, rest);
                return fa_pair_create(first, fa_pair_create(interval, new_if));
            } else {
                if (interval) fa_destroy(interval);
                return fa_pair_create(first, NULL);
            }
        } else {
            fa_deep_destroy_always(first);
            if (rest) {
                return fa_pair_create(fa_action_null(), fa_pair_create(interval, fa_action_if(pred_function, pred_data, rest)));
            } else {
                if (interval) fa_destroy(interval);
                return NULL;
            }

        }
    }
}

static inline fa_ptr_t _while(fa_ptr_t data, fa_ptr_t c)
{
    fa_action_t   compound = c;
    fa_pair_t     pred_action = data;
    fa_func_ref_t pred_closure;
    fa_pred_t     pred_function;
    fa_ptr_t      pred_data;
    action_t      action;
    unpair(pred_closure, action, pred_action);
    pred_function = fa_func_ref_func(pred_closure);
    pred_data = fa_func_ref_data(pred_closure);
    fa_destroy(pred_closure);
    compound_get(compound, data) = NULL; // reset reference, to avoid double free
    
    if (fa_action_is_simple(action)) {
        if (pred_function(pred_data, NULL)) {
            return fa_pair_create(action, NULL);
        } else {
            fa_action_destroy(action);
            return NULL;
        }
    } else {
        action_t  first;
        fa_time_t interval;
        action_t  rest;
        compound_render(action, &first, &interval, &rest);
        
        fa_action_destroy(action);
        
        if (!first && !rest) {
            return NULL;
        }

        if (pred_function(pred_data, NULL)) {
            if (rest) {
                return fa_pair_create(first, (fa_pair_create(interval, fa_action_while(pred_function, pred_data, rest))));
            } else {
                if (interval) fa_destroy(interval);
                return fa_pair_create(first, NULL);
            }
        } else {
            fa_deep_destroy_always(first);
            if (rest) fa_deep_destroy_always(rest);
            if (interval) fa_destroy(interval);
            return NULL;
        }
    }
}

static inline fa_ptr_t _until(fa_ptr_t data, fa_ptr_t c)
{
    fa_action_t   compound = c;
    fa_pair_t     pred_action = data;
    fa_func_ref_t pred_closure;
    fa_pred_t     pred_function;
    fa_ptr_t      pred_data;
    action_t      action;
    unpair(pred_closure, action, pred_action);
    pred_function = fa_func_ref_func(pred_closure);
    pred_data = fa_func_ref_data(pred_closure);
    fa_destroy(pred_closure);
    compound_get(compound, data) = NULL; // reset reference, to avoid double free
    
    if (fa_action_is_simple(action)) {
        if (!pred_function(pred_data, NULL)) {
            return fa_pair_create(action, NULL);
        } else {
            fa_action_destroy(action);
            return NULL;
        }
    } else {
        action_t  first;
        fa_time_t interval;
        action_t  rest;
        compound_render(action, &first, &interval, &rest);
        
        fa_action_destroy(action);
        
        if (!first && !rest) {
            return NULL;
        }

        if (!pred_function(pred_data, NULL)) {
            if (rest) {
                return fa_pair_create(first, (fa_pair_create(interval, fa_action_until(pred_function, pred_data, rest))));
            } else {
                if (interval) fa_destroy(interval);
                return fa_pair_create(first, NULL);
            }
        } else {
            fa_deep_destroy_always(first);
            if (rest) fa_deep_destroy_always(rest);
            if (interval) fa_destroy(interval);
            return NULL;
        }
    }
}


// [(Action, Time)] -> Action
fa_action_t fa_action_if(fa_pred_t pred, fa_ptr_t data, fa_action_t action)
{
    return fa_action_compound(_if, fa_pair_create(fa_func_ref_create(pred, data), action));
}

fa_action_t fa_action_while(fa_pred_t pred, fa_ptr_t data, fa_action_t action)
{
    return fa_action_compound(_while, fa_pair_create(fa_func_ref_create(pred, data), action));
}

fa_action_t fa_action_until(fa_pred_t pred, fa_ptr_t data, fa_action_t action)
{
    return fa_action_compound(_until, fa_pair_create(fa_func_ref_create(pred, data), action));
}


static inline bool is_due (fa_time_t time, fa_time_t now) {
    return fa_time_to_milliseconds(now) > (fa_time_to_milliseconds(time) - kScheduleLookahead);
}

static inline bool is_too_old (fa_time_t time, fa_time_t now) {
    return fa_time_to_milliseconds(now) > (fa_time_to_milliseconds(time) + kScheduleMaxAge);
}

/**
    Run a single or compound action, pushing to the given rescheduling list of needed.
    @param
        action  Action to run.
        time    Current time (for rescheduling).
        resched A list to which a (time, action) values is pushed for each rescheduled action.
        state   State to run action on.
 */
void run_and_resched_action(action_t action, fa_time_t time, fa_time_t now, fa_list_t *resched, fa_binary_t function, fa_ptr_t data)
{
    //fa_slog_info("run_and_resched_action ", time);
    //fa_slog_info("run_and_resched   in thread:   ", fa_string_format_integral("%p", (long) pthread_self()));
    //printf("run_and_resched   %p\n", pthread_self());
    if (fa_action_is_compound(action)) {

        action_t  first;
        fa_time_t interval;
        action_t  rest;
        compound_render(action, &first, &interval, &rest);

        //fa_slog_info("   first, rest, interval: ", first, rest, interval);
        
        //fa_slog_info("run_and_resched ", fa_string_format_integral("%p", (long) fa_thread_current()));

        if (first) {
            run_and_resched_action(first, fa_copy(time), now, resched, function, data);
        }

        if (rest && interval) {
            fa_time_t future = fa_add(time, interval);
            fa_destroy(interval);

            if (is_too_old(future, now)) {
                // Discard
                fa_slog_info("Too old, discarding", future, now);
                fa_deep_destroy_always(rest);
            } else if (is_due(future, now)) {
                // Run directly
                run_and_resched_action(rest, future, now, resched, function, data);
            } else {
                // Reschedule
                //fa_slog_info("   rescheduling ", rest);
                fa_push_list(fa_pair_left_create(future, rest), *resched);
            }
        }
        fa_action_destroy(action); // Shallow destroy, only the compound
        fa_destroy(time);
        
        return;
    } else {
        if (is_do(action)) {
            fa_nullary_t           do_function           = do_get(action, function);
            nullary_with_time_t    do_function_with_time = do_get(action, function_with_time);
            fa_ptr_t               do_data               = do_get(action, data);

            if (do_function) {
                do_function(do_data);
            }

            if (do_function_with_time) {
                do_function_with_time(do_data, time, now);
            }
        } else {
            //fa_slog_info("Running action ", action);
            //fa_slog_info("Running action  ", fa_string_format_integral("%p", (long) fa_thread_current()));
            function(data, action, time);
            // Note: /function/ is responsible for releasing the actions when used.
            // We can't do it here, as /function/ is run in another thread.
        }
        
        fa_destroy(time);
        
        return;
    }

    assert(false && "Unreachable");
}


//static int32_t last_time = 0;
//static int32_t last_now = 0;

/**
    Run all due actions in the given queue.
    @param
        controls    A priority queue of (time, action) values.
        now         Current time (not destroyed).
        function    Function to which due actions are passed.
 */
void run_actions(fa_priority_queue_t controls, fa_time_t now, fa_binary_t function, fa_ptr_t data)
{
    //printf("run_actions %lld\n", (int64_t) fa_time_to_milliseconds(now));    
    while (1) {
        fa_pair_t x = fa_priority_queue_peek(controls);

        // Assure there is a next action
        if (!x) {
            break;
        }

        fa_time_t time  = fa_pair_first(x);
        action_t action = fa_pair_second(x);

        // Assure the next action is due
        if (is_due(time, now)) {
            fa_priority_queue_pop(controls);
            fa_destroy(x);
            
            if (is_too_old(time, now)) {
                fa_slog_info("Too old, discarding ", time, now);
                fa_deep_destroy_always(action);
                fa_destroy(time);
                continue;
            }
            
            //printf("  run_actions  %lld  %lld\n", (int64_t) fa_time_to_milliseconds(time), (int64_t) fa_time_to_milliseconds(now));
            //printf("run_actions %p\n", pthread_self());
            //   (int32_t) fa_time_to_milliseconds(now) - last_now,
            //   (int32_t) fa_time_to_milliseconds(time) - last_time);
            //last_now = (int32_t) fa_time_to_milliseconds(now);
            // last_time = (int32_t) fa_time_to_milliseconds(time);

            fa_list_t resched = fa_list_empty();

            // Run action, generating list of actions to reschedule
            run_and_resched_action(action, time, now, &resched, function, data); // TODO

            if (fa_list_is_empty(resched)) {
                //fa_destroy(time);
            } else {
                fa_for_each(x, resched) {
                    fa_priority_queue_insert(x, controls);
                }
            }
            fa_destroy(resched);
        } else {
            break;
        }
    }
}


// --------------------------------------------------------------------------------

fa_ptr_t action_copy(fa_ptr_t a)
{
    return fa_action_copy(a);
}

fa_ptr_t action_deep_copy(fa_ptr_t a)
{
    return fa_action_copy(a); // actions are always deep-copied
}

void action_destroy(fa_ptr_t a)
{
    return fa_action_destroy(a);
}

void action_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    return fa_action_deep_destroy(a, p);
}

fa_string_t action_show(fa_ptr_t a)
{
    action_t x = (action_t) a;

    fa_string_t str = fa_string("<Action ");
    switch (x->tag) {
    case get_action:      str = fa_string_dappend(str, fa_string("get")); break;
    case set_action:      str = fa_string_dappend(str, fa_string("set")); break;
    case accum_action:    str = fa_string_dappend(str, fa_string("accum")); break;
    case send_action:     str = fa_string_dappend(str, fa_string("send")); break;
    case do_action:       str = fa_string_dappend(str, fa_string("do")); break;
    case compound_action:
        if (compound_get(x, function) == _null) {
            str = fa_string_dappend(str, fa_string("null"));
        } else if (compound_get(x, function) == _many) {
            str = fa_string_dappend(str, fa_string("many"));
        } else if (compound_get(x, function) == _repeat) {
            str = fa_string_dappend(str, fa_string("repeat"));
        } else if (compound_get(x, function) == _if) {
            str = fa_string_dappend(str, fa_string("if"));
        } else if (compound_get(x, function) == _while) {
            str = fa_string_dappend(str, fa_string("while"));
        } else if (compound_get(x, function) == _until) {
            str = fa_string_dappend(str, fa_string("until"));
        } else {
            str = fa_string_dappend(str, fa_dappend(fa_string("other compound "), fa_string_show(compound_get(x, data))));
        }
        break;
    }
    //str = fa_string_dappend(str, fa_string(" "));
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) x));
    //str = fa_string_dappend(str, fa_string_format_integral(" %d", x->ref_count));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

fa_dynamic_type_repr_t action_get_type(fa_ptr_t a)
{
    return action_type_repr;
}

fa_ptr_t action_impl(fa_id_t interface)
{
    static fa_copy_t action_copy_impl
        = { action_copy, action_deep_copy };
    static fa_destroy_t action_destroy_impl
        = { action_destroy, action_deep_destroy };
    static fa_string_show_t action_show_impl
        = { action_show };
    static fa_dynamic_t action_dynamic_impl
        = { action_get_type };

    switch (interface) {
    case fa_copy_i:
        return &action_copy_impl;

    case fa_destroy_i:
        return &action_destroy_impl;

    case fa_string_show_i:
        return &action_show_impl;
        
    case fa_dynamic_i:
        return &action_dynamic_impl;

    default:
        return NULL;
    }
}
struct entry {
    fa_impl_t      impl;       //  Interface dispatcher
    fa_ptr_t       key;        //  Values
    fa_ptr_t       value;
};

typedef struct entry *entry_t;

struct _fa_map_t {
    fa_impl_t       impl;       //  Interface dispatcher
    fa_set_t        entries;    //  Set of entries
};


// static fa_ptr_t _show_action(fa_ptr_t data, entry_t entry)
// {
//     return fa_string_dappend(fa_string_show((fa_ptr_t)fa_peek_int64(entry->key)), fa_string_dappend(fa_string(": "), fa_string_show(entry->value)));
// }
//
// void print_all_actions()
// {
//     fa_dlog_info(fa_string_show(fa_list_map((fa_unary_t) _show_action, NULL, fa_set_to_list(all_actions->entries))));
// }
        
// void print_all_actions()
// {
//     fa_for_each(a, all_actions) {
//         //fa_dlog_info(fa_dappend(fa_string_show(a), fa_dappend(fa_string(": "), fa_string_show(fa_i8(((fa_action_t)a)->ref_count)))));
//         fa_dlog_info(fa_string_show(a));
//      }
// }

void fa_action_log_count()
{
    fa_log_info(fa_string_dappend(fa_string("Actions allocated: "), fa_string_dshow(fa_i32(gActionCount))));
}
