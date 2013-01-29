
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/event.h>
#include <doremir/set.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*
    Notes:
 */
struct _doremir_event_t {
    
    impl_t      impl;

    enum {
        never_event,      // E a
        now_event,        // a -> E a
        delay_event,      // t -> E a -> E a
        merge_event,     // E a -> E a -> E a
        switch_event      // E a -> E b -> E b -> E b
    }                       tag;

    union {
        struct {
        }                   never;
        struct {
            ptr_t           value;
        }                   now;
        struct {
            ptr_t           event;
            time_t          time;
        }                   delay;
        struct {
            ptr_t           left;
            ptr_t           right;
        }                   merge ;
        struct {
            ptr_t           pred;
            ptr_t           before;
            ptr_t           after;
        }                   switch_;
    }                       fields;
};

// --------------------------------------------------------------------------------


doremir_event_t new_event(int tag)
{
    event_t e = doremir_new(event);
    doremir_ptr_t event_impl(doremir_id_t interface);

    e->impl = &event_impl;
    e->tag  = tag;

    return e;
}

void delete_event(doremir_event_t event)
{
    doremir_delete(event);
}

#define now_get(v,f)      v->fields.now.f
#define delay_get(v,f)    v->fields.delay.f
#define merge_get(v,f)   v->fields.merge.f
#define switch_get(v,f)   v->fields.switch_.f

// --------------------------------------------------------------------------------

doremir_event_t doremir_event_never()
{
    event_t e = new_event(never_event);
    return e;
}

doremir_event_t doremir_event_now(doremir_ptr_t value)
{
    event_t e = new_event(now_event);
    now_get(e, value) = value;
    return e;
}

doremir_event_t doremir_event_delay(doremir_time_t time,
                                    doremir_event_t event)
{
    event_t e = new_event(delay_event);
    delay_get(e, time)  = time;
    delay_get(e, event) = event;
    return e;
}

doremir_event_t doremir_event_merge(doremir_event_t event1,
                                     doremir_event_t event2)
{
    event_t e = new_event(merge_event);
    merge_get(e, left)   = event1;
    merge_get(e, right)  = event2;
    return e;
}

doremir_event_t doremir_event_switch(doremir_event_t pred,
                                     doremir_event_t event1,
                                     doremir_event_t event2)
{
    event_t e = new_event(switch_event);
    switch_get(e, pred)     = pred;
    switch_get(e, before)   = event1;
    switch_get(e, after)    = event2;
    return e;
}

void doremir_event_destroy(doremir_event_t event)
{
    // TODO manage components
    delete_event(event);
}

#define TIME_ZERO doremir_time_create(0,0,0,ratio(0,1))
#define TIME_MAX  doremir_time_create(2000000,0,0,ratio(0,1)) // FIXME

doremir_time_t doremir_event_offset(doremir_event_t event)
{
    switch (event->tag) {

        case never_event:
            return TIME_MAX;

        case now_event:
            return TIME_ZERO;

        case delay_event: {
            time_t dx  = doremir_event_offset(delay_get(event, event));
            time_t t = delay_get(event, time);
            return doremir_add(dx, t);
        }

        case merge_event: {
            time_t x = doremir_event_offset(merge_get(event, left));
            time_t y = doremir_event_offset(merge_get(event, right));
            return doremir_min(x, y);
        }

        case switch_event: {
            time_t x = doremir_event_offset(switch_get(event, before));
            time_t y = doremir_event_offset(switch_get(event, after));
            return doremir_min(x, y);
        }

        default:
            assert(false && "Missing label");
    }
}

bool doremir_event_has_value(doremir_event_t event, doremir_time_t time)
{
    switch (event->tag) {
        
        case never_event:
            return false;

        case now_event:
            return doremir_greater_than_equal(time, seconds(0));

        case delay_event:
            return doremir_event_has_value(
                       delay_get(event, event),
                       doremir_subtract(time, delay_get(event, time)));

        case merge_event:
            return doremir_event_has_value(merge_get(event, left), time)
                   || doremir_event_has_value(merge_get(event, right), time);

        case switch_event:
            return doremir_event_has_value(switch_get(event, pred), time)
                   ? doremir_event_has_value(switch_get(event, before), time)
                   : doremir_event_has_value(switch_get(event, after), time);

        default:
            assert(false && "Missing label");
    }
}

doremir_ptr_t doremir_event_value(doremir_event_t event)
{
    switch (event->tag) {
        
        case never_event:
            return NULL;

        case now_event:
            return now_get(event, value);

        case delay_event:
            return doremir_event_value(delay_get(event, event));

        case merge_event:
            return doremir_event_value(merge_get(event, left)); // FIXME

        case switch_event:
            return NULL;

        default:
            assert(false && "Missing label");
    }
}

doremir_event_t doremir_event_tail(doremir_event_t event)
{
    switch (event->tag) {
        
        case never_event:
            return NULL;

        case now_event:
            return NULL;

        case delay_event: {
            event_t x = doremir_event_tail(delay_get(event, event));
            time_t  t = delay_get(event, time);

            if (!x) {
                return NULL;
            } else {
                return doremir_event_delay(t, x);
            }
        }

        case merge_event:
            return merge_get(event, right); // FIXME

        case switch_event:
            return NULL;

        default:
            assert(false && "Missing label");
    }
}


doremir_event_t doremir_event_external()
{
    assert(false && "Not implemented");
}

void doremir_event_trig(doremir_event_t event)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

bool event_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    event_t c = (event_t) a;
    event_t d = (event_t) b;
    return doremir_equal(
               doremir_event_offset(c),
               doremir_event_offset(d));
}

bool event_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    event_t c = (event_t) a;
    event_t d = (event_t) b;
    return doremir_less_than(
               doremir_event_offset(c),
               doremir_event_offset(d));
}

bool event_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    return event_less_than(b, a);
}

string_t event_show(doremir_ptr_t a)
{
    event_t event = (event_t) a;
    string_t s = string("");

    switch (event->tag) {
        
        case never_event:
            return string("<Never>");

        case now_event:
            return string("<Now>");

        case delay_event: {
            time_t  t = delay_get(event, time);
            event_t x  = delay_get(event, event);

            write_to(s, string("<Delay "));
            write_to(s, format_int("%d", doremir_time_seconds(t))); // FIXME
            write_to(s, string(" "));
            write_to(s, doremir_string_show(x));
            write_to(s, string(">"));

            return s;
        }

        case merge_event: {
            event_t x = merge_get(event, left);
            event_t y = merge_get(event, right);

            write_to(s, string("<Either "));
            write_to(s, doremir_string_show(x));
            write_to(s, string(" "));
            write_to(s, doremir_string_show(y));
            write_to(s, string(">"));

            return s;
        }

        case switch_event: {
            event_t p = switch_get(event, pred);
            event_t x = switch_get(event, before);
            event_t y = switch_get(event, after);

            write_to(s, string("<Switch "));
            write_to(s, doremir_string_show(p));
            write_to(s, string(" "));
            write_to(s, doremir_string_show(x));
            write_to(s, string(" "));
            write_to(s, doremir_string_show(y));
            write_to(s, string(">"));

            return s;

        default:
            assert(false && "Missing label");
        }
    }

    return s;
}

void event_destroy(doremir_ptr_t a)
{
    doremir_event_destroy(a);
}

doremir_ptr_t event_impl(doremir_id_t interface)
{
    static doremir_equal_t event_equal_impl = { event_equal };
    static doremir_order_t event_order_impl = { event_less_than, event_greater_than };
    static doremir_string_show_t event_show_impl = { event_show };
    static doremir_destroy_t event_destroy_impl = { event_destroy };

    switch (interface) {
        case doremir_equal_i:
            return &event_equal_impl;

        case doremir_order_i:
            return &event_order_impl;

        case doremir_string_show_i:
            return &event_show_impl;

        case doremir_destroy_i:
            return &event_destroy_impl;

        default:
            return NULL;
    }
}

