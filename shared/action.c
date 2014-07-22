
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/action.h>
#include <fa/util.h>
#include <fa/pair/left.h>
#include <fa/priority_queue.h>

typedef fa_action_t                 action_t;
typedef fa_action_channel_t         channel_t;
typedef fa_action_name_t            name_t;
typedef fa_signal_unary_double_t    unary_double_t;

struct _fa_action_t {

    fa_impl_t                       impl;

    enum {
        set_action,
        accum_action,
        send_action,
        do_action,
        compound_action
    }                               tag;

    union {
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
        }                           send;

        struct {
            fa_nullary_t            function;
            fa_ptr_t                data;
        }                           do_;

        struct {
            // Action -> NULL or (SimpleAction, (Time, Action))
            fa_unary_t              function;
            fa_ptr_t                data;
        }                           compound;


    }                               fields;
};

inline static action_t new_action(int tag)
{
    fa_ptr_t action_impl(fa_id_t interface);

    action_t s = fa_new(action);
    s->impl = &action_impl;
    s->tag  = tag;
    return s;
}

inline static void delete_action(action_t action)
{
    fa_delete(action);
}

#define is_set(v)           (v->tag == set_action)
#define is_accum(v)         (v->tag == accum_action)
#define is_send(v)          (v->tag == send_action)
#define is_do(v)            (v->tag == do_action)
#define is_compound(v)      (v->tag == compound_action)

#define set_get(v,f)        v->fields.set.f
#define accum_get(v,f)      v->fields.accum.f
#define send_get(v,f)       v->fields.send.f
#define do_get(v,f)         v->fields.do_.f
#define compound_get(v,f)   v->fields.compound.f

// --------------------------------------------------------------------------------

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
    action_t action = new_action(send_action);
    send_get(action, name)  = fa_copy(name);
    send_get(action, value) = value;
    return action;
}

fa_action_t fa_action_do(fa_nullary_t function, fa_ptr_t data)
{
    action_t action = new_action(do_action);
    do_get(action, function)  = function;
    do_get(action, data)      = data;
    return action;
}

fa_action_t fa_action_compound(fa_unary_t function, fa_ptr_t data)
{
    action_t action = new_action(compound_action);
    compound_get(action, function)  = function;
    compound_get(action, data)      = data;
    return action;
}


fa_action_t fa_action_copy(fa_action_t action)
{
    assert(false && "Not implemented");
}

void fa_action_destroy(fa_action_t action)
{
    // TODO
    delete_action(action);
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



// Action -> Maybe (Maybe Action, Maybe (Time, Action))
fa_pair_t compound_render(fa_action_t action)
{
    assert(is_compound(action) && "Not a compound action");
    return compound_get(action, function)(compound_get(action, data), action);
}

fa_action_t fa_action_compound_first(fa_action_t action)
{
    fa_pair_t maybeFirstRest = compound_render(action);

    if (maybeFirstRest) {
        return fa_pair_first(maybeFirstRest);
    } else {
        return NULL;
    }
}

fa_time_t fa_action_compound_interval(fa_action_t action)
{
    fa_pair_t maybeFirstRest = compound_render(action);

    if (maybeFirstRest) {
        fa_pair_t maybeRest = fa_pair_second(maybeFirstRest);

        if (maybeRest) {
            return fa_pair_first(maybeRest);
        }
    }

    return NULL;
}

fa_action_t fa_action_compound_rest(fa_action_t action)
{
    fa_pair_t maybeFirstRest = compound_render(action);

    if (maybeFirstRest) {
        fa_pair_t maybeRest = fa_pair_second(maybeFirstRest);

        if (maybeRest) {
            return fa_pair_second(maybeRest);
        }
    }

    return NULL;
}



static inline fa_ptr_t _null(fa_ptr_t data, fa_ptr_t compound)
{
    return NULL;
}

fa_action_t fa_action_null()
{
    return fa_action_compound(_null, NULL);
}



#define unpair(x,y,p) fa_pair_decons((fa_ptr_t*) &x, (fa_ptr_t*) &y, p)

static inline fa_ptr_t _repeat(fa_ptr_t data, fa_ptr_t compound)
{
    fa_time_t interval;
    action_t simple;
    unpair(interval, simple, data);
    return fa_pair_create(simple, fa_pair_create(interval, compound));
}

fa_action_t fa_action_repeat(fa_time_t interval, fa_action_t action)
{
    return fa_action_compound(_repeat, fa_pair_create(interval, action));
}



static inline fa_ptr_t _many(fa_ptr_t data, fa_ptr_t compound)
{
    fa_list_t timeActions = data;

    if (fa_list_is_empty(timeActions)) {
        return NULL;
    } else {
        action_t first;
        fa_time_t   interval;
        unpair(first, interval, fa_list_head(timeActions));

        action_t rest = fa_action_many(fa_list_tail(timeActions));
        return fa_pair_create(first, fa_pair_create(interval, rest));
    }
}

// [(Action, Time)] -> Action
fa_action_t fa_action_many(fa_list_t timeActions)
{
    return fa_action_compound(_many, timeActions);
}



static inline fa_ptr_t _if(fa_ptr_t data, fa_ptr_t compound)
{
    fa_pair_t      pred_action = data;

    fa_pair_t      pred_closure;
    fa_pred_t      pred_function;
    fa_ptr_t       pred_data;
    action_t    action;
    unpair(pred_closure, action, pred_action);
    unpair(pred_function, pred_data, pred_closure);

    if (fa_action_is_simple(action)) {
        if (pred_function(pred_data, NULL)) {
            return fa_pair_create(action, NULL);
        } else {
            return NULL;
        }
    } else {
        action_t first    = fa_action_compound_first(action);
        action_t rest     = fa_action_compound_rest(action);
        fa_time_t   interval = fa_action_compound_interval(action);

        if (pred_function(pred_data, NULL)) {
            return fa_pair_create(first,            fa_pair_create(interval, fa_action_if(pred_function, pred_data, rest)));
        } else {
            return fa_pair_create(fa_action_null(), fa_pair_create(interval, fa_action_if(pred_function, pred_data, rest)));
        }
    }
}

static inline fa_ptr_t _while(fa_ptr_t data, fa_ptr_t compound)
{
    fa_pair_t pred_action = data;
    fa_pair_t      pred_closure;
    fa_pred_t      pred_function;
    fa_ptr_t       pred_data;
    action_t    action;
    unpair(pred_closure, action, pred_action);
    unpair(pred_function, pred_data, pred_closure);

    if (fa_action_is_simple(action)) {
        if (pred_function(pred_data, NULL)) {
            return fa_pair_create(action, NULL);
        } else {
            return NULL;
        }
    } else {
        action_t first    = fa_action_compound_first(action);
        action_t rest     = fa_action_compound_rest(action);
        fa_time_t   interval = fa_action_compound_interval(action);

        if (pred_function(pred_data, NULL)) {
            return fa_pair_create(first,            fa_pair_create(interval, fa_action_while(pred_function, pred_data, rest)));
        } else {
            return NULL;
        }
    }
}

static inline fa_ptr_t _until(fa_ptr_t data, fa_ptr_t compound)
{
    fa_pair_t pred_action = data;
    fa_pair_t      pred_closure;
    fa_pred_t      pred_function;
    fa_ptr_t       pred_data;
    action_t    action;
    unpair(pred_closure, action, pred_action);
    unpair(pred_function, pred_data, pred_closure);

    if (fa_action_is_simple(action)) {
        if (!pred_function(pred_data, NULL)) {
            return fa_pair_create(action, NULL);
        } else {
            return NULL;
        }
    } else {
        action_t first    = fa_action_compound_first(action);
        action_t rest     = fa_action_compound_rest(action);
        fa_time_t   interval = fa_action_compound_interval(action);

        if (!pred_function(pred_data, NULL)) {
            return fa_pair_create(first,            fa_pair_create(interval, fa_action_until(pred_function, pred_data, rest)));
        } else {
            return NULL;
        }
    }
}


// [(Action, Time)] -> Action
fa_action_t fa_action_if(fa_pred_t pred, fa_ptr_t data, fa_action_t action)
{
    return fa_action_compound(_if, fa_pair_create(fa_pair_create(pred, data), action));
}

fa_action_t fa_action_while(fa_pred_t pred, fa_ptr_t data, fa_action_t action)
{
    return fa_action_compound(_while, fa_pair_create(fa_pair_create(pred, data), action));
}

fa_action_t fa_action_until(fa_pred_t pred, fa_ptr_t data, fa_action_t action)
{
    return fa_action_compound(_until, fa_pair_create(fa_pair_create(pred, data), action));
}



/**
    Run a single or compound action, pushing to the given rescheduling list of needed.
    @param
        action  Action to run.
        time     Current time (for rescheduling).
        resched A list to which a (time, action) values is pushed for each rescheduled action.
        state   State to run action on.
 */
void run_and_resched_action(action_t action, fa_time_t time, fa_time_t now, fa_list_t *resched, fa_unary_t function, fa_ptr_t data)
{
    if (fa_action_is_compound(action)) {

        action_t first  = fa_action_compound_first(action);
        action_t rest   = fa_action_compound_rest(action);
        fa_time_t   interv = fa_action_compound_interval(action);

        if (first) {
            run_and_resched_action(first, time, now, resched, function, data);
        }

        if (rest && interv) {
            fa_time_t   future = fa_add(time, interv);

            if (fa_less_than_equal(future, now)) {
                // Run directly
                run_and_resched_action(rest, future, now, resched, function, data);
            } else {
                // Reschedule
                fa_push_list(fa_pair_left_create(future, rest), *resched);
            }
        }

        return;
    } else {
        // TODO should this always happen here?
        if (is_do(action)) {
            fa_nullary_t function  = do_get(action, function);
            fa_ptr_t     data      = do_get(action, data);
            function(data);
        }

        // printf("Running!\n");
        function(data, action);
        return;
    }

    assert(false && "Unreachable");
}


/**
    Run all due actions in the given queue.
    @param
        controls    A priority queue of (time, action) values.
        time        Current time (not destroyed).
        function    Function to which due actions are passed.
 */
void run_actions(fa_priority_queue_t controls, fa_time_t now, fa_unary_t function, fa_ptr_t data)
{
    while (1) {
        fa_pair_t x = fa_priority_queue_peek(controls);

        // Assure there is a next action
        if (!x) {
            break;
        }

        fa_time_t   time        = fa_pair_first(x);
        action_t action      = fa_pair_second(x);

        // Assure the next action is due
        if (fa_less_than_equal(time, now)) {
            fa_priority_queue_pop(controls);

            fa_list_t resched = fa_empty();

            // Run action, generating list of actions to reschedule
            run_and_resched_action(action, time, now, &resched, function, data); // TODO

            fa_for_each(x, resched) {
                fa_priority_queue_insert(x, controls);
            }
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

void action_destroy(fa_ptr_t a)
{
    return fa_action_destroy(a);
}

fa_string_t action_show(fa_ptr_t a)
{
    action_t x = (action_t) a;

    fa_string_t str = fa_string("<Action ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) x));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

fa_ptr_t action_impl(fa_id_t interface)
{
    static fa_copy_t action_copy_impl
        = { action_copy };
    static fa_destroy_t action_destroy_impl
        = { action_destroy };
    static fa_string_show_t action_show_impl
        = { action_show };

    switch (interface) {
    case fa_copy_i:
        return &action_copy_impl;

    case fa_destroy_i:
        return &action_destroy_impl;

    case fa_string_show_i:
        return &action_show_impl;

    default:
        return NULL;
    }
}

