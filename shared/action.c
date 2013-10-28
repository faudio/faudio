
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/action.h>
#include <fa/util.h>

typedef fa_action_t                 action_t;
typedef fa_action_channel_t         channel_t;
typedef fa_action_name_t            name_t;
typedef fa_signal_unary_double_t    unary_double_t;

struct _fa_action_t {

    impl_t                      impl;

    enum {
        set_action,
        accum_action,
        send_action
    }                           tag;

    union {
        struct {
            channel_t           channel;
            double              value;
        }                       set;

        struct {
            channel_t           channel;
            unary_double_t      function;
            ptr_t               data;
        }                       accum;

        struct {
            name_t              name;
            ptr_t               value;
        }                       send;
    }                       fields;
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

#define set_get(v,f)        v->fields.set.f
#define accum_get(v,f)      v->fields.accum.f
#define send_get(v,f)       v->fields.send.f

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
    send_get(action, name)  = name;
    send_get(action, value) = value;
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





// --------------------------------------------------------------------------------

ptr_t action_copy(ptr_t a)
{
    return fa_action_copy(a);
}

void action_destroy(ptr_t a)
{
    return fa_action_destroy(a);
}

string_t action_show(ptr_t a)
{
    action_t x = (action_t) a;

    string_t str = string("<Action ");
    str = string_dappend(str, fa_string_format_integral(" %p", (long) x));
    str = string_dappend(str, string(">"));
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

