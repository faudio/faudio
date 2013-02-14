
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/event.h>
#include <doremir/set.h>
#include <doremir/string.h>
#include <doremir/message.h>
#include <doremir/util.h>

// TODO rename, make public
#define TIME_ZERO doremir_time_create(0,0,0,ratio(0,1))
#define TIME_MAX  doremir_time_create(2000000,0,0,ratio(0,1)) // FIXME

/*
    Notes:
 */
struct _doremir_event_t {

    impl_t      impl;

    enum {
        never_event,      // E a
        now_event,        // a -> E a
        delay_event,      // t -> E a -> E a
        merge_event,      // E a -> E a -> E a
        switch_event,     // E a -> E b -> E b -> E b
        send_event,       // D x a -> x -> E a -> E ()
        recv_event        // D x a -> x -> E a
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
        struct {
            ptr_t           dispatcher;
            ptr_t           address;
            ptr_t           event;
        }                   send;
        struct {
            ptr_t           dispatcher;
            ptr_t           address;
        }                   recv;

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

#define is_never(v)       (v->tag == never_event)
#define is_now(v)         (v->tag == now_event)
#define is_delay(v)       (v->tag == delay_event)
#define is_merge(v)       (v->tag == merge_event)
#define is_switch(v)      (v->tag == switch_event)
#define is_send(v)        (v->tag == send_event)
#define is_recv(v)        (v->tag == recv_event)

#define now_get(v,f)      (v->fields.now.f)
#define delay_get(v,f)    (v->fields.delay.f)
#define merge_get(v,f)    (v->fields.merge.f)
#define switch_get(v,f)   (v->fields.switch_.f)
#define send_get(v,f)     (v->fields.send.f)
#define recv_get(v,f)     (v->fields.recv.f)

// --------------------------------------------------------------------------------

/** Create an empty event.
    This event never occurs.
    @return         A new event.
 */
doremir_event_t doremir_event_never()
{
    event_t e = new_event(never_event);
    return e;
}

/** Create a single event, occuring directly.
    @return         A new event.
 */
doremir_event_t doremir_event_now(doremir_ptr_t value)
{
    event_t e = new_event(now_event);
    now_get(e, value) = value;
    return e;
}

/** Create a single event, occuring after the given time has passed.
    @return         A new event.
 */
doremir_event_t doremir_event_later(doremir_time_t u, doremir_ptr_t x)
{
    return doremir_event_delay(u, doremir_event_now(x));
}

/** Delay an event by the given amount of time.

    @param time     Amount of time to delay.
    @param event    Event to delay.
    @return         A new event.

    @par Laws

        delay t (delay u)   = delay (t + u)
        delay t (merge x y) = delay t x `merge` delay t u

 */
doremir_event_t doremir_event_delay(doremir_time_t  time,
                                    doremir_event_t event)
{

    assert(event && "Can not delay null");

    if (is_delay(event)) {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return doremir_event_delay(doremir_add(time, t), x);
    }

    event_t e = new_event(delay_event);
    delay_get(e, time)  = time;
    delay_get(e, event) = event;
    return e;
}

/** Merge two events.

    The resulting event has all occurances of the given events interleaved.

    @param event1   Event to merge.
    @param event2   Event to merge.
    @return         A new event.

    @par Laws

        merge (switch p x y) (switch p y x) = merge x y
        merge (merge x y) z = merge x (merge y z)
        merge x never       = x
        merge x y           = merge y x

 */
doremir_event_t doremir_event_merge(doremir_event_t event1,
                                    doremir_event_t event2)
{
    assert(event1 && "Can not merge null");
    assert(event2 && "Can not merge null");

    if (doremir_event_is_never(event1))
        return event2;
    if (doremir_event_is_never(event2))
        return event1;

    // invariant left <= right
    event_t e = new_event(merge_event);

    if (doremir_less_than(event1, event2)) {
        merge_get(e, left)   = event1;
        merge_get(e, right)  = event2;
    } else {
        merge_get(e, left)   = event2;
        merge_get(e, right)  = event1;
    }

    return e;
}

/** Create an event that switches from one event to another at a the first
    occurance of a trigger event.

    @param trigger  Triggering event.
    @param event1   Behave as this event before the trigger occurs.
    @param event2   Behave as this event after the trigger occurs.
    @return         A new event.

    @par Laws

        switch x never x    = x
        switch (now _) x y  = y
        switch x x never    = never
        switch never x y    = x
 */
doremir_event_t doremir_event_switch(doremir_event_t pred,
                                     doremir_event_t event1,
                                     doremir_event_t event2)
{
    assert(pred   && "Can not switch null");
    assert(event2 && "Can not switch null");
    assert(event2 && "Can not switch null");

    event_t e = new_event(switch_event);
    switch_get(e, pred)     = pred;
    switch_get(e, before)   = event1;
    switch_get(e, after)    = event2;
    return e;
}

/** Create an event that sends values to the given receiver. Each occurence
    of the given event results in its value being sent to the given receiver,
    and the returned event occuring with value null.

    @param sender   Sender to receive from.
    @param address  Address to receive on.
    @param event    Event from which to obtain values.
    @return         An event occuring whenever a message is being sent.
 */
doremir_event_t doremir_event_send(doremir_message_receiver_t   receiver,
                                   doremir_message_address_t    address,
                                   doremir_event_t              event)
{
    assert(receiver  && "Need a receiver");
    assert(address   && "Need an address");
    assert(event     && "Need an event");

    event_t e = new_event(send_event);
    send_get(e, dispatcher) = receiver;
    send_get(e, address)    = address;
    send_get(e, event)      = event;
    return e;
}

/** Create an event that receives values from the given sender.
    @param sender   Sender to receive from.
    @param address  Address to receive on.
    @return         An event occuring whenever a message has been received.
 */
doremir_event_t doremir_event_receive(doremir_message_sender_t  sender,
                                      doremir_message_address_t address)
{
    assert(sender  && "Need a sender");

    event_t e = new_event(recv_event);
    recv_get(e, dispatcher) = sender;
    recv_get(e, address)    = address;
    return e;
}

/** Destroy the given event.
 */
void doremir_event_destroy(doremir_event_t event)
{
    delete_event(event);
}







doremir_time_t doremir_event_offset(doremir_event_t event)
{
    // offset never             = infinity
    // offset now               = 0
    // offset (delay x t)       = (offset x) + t
    // offset (merge x y)       = min (offset x) (offset y)
    // offset (switch p x y)    = min (offset x) (offset y)
    // offset (send x)          = offset x
    // offset (recv x)          = 0

    switch (event->tag) {

    case never_event:
        return TIME_MAX;

    case now_event:
        return TIME_ZERO;

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return doremir_add(doremir_event_offset(x), t);
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        return doremir_min(doremir_event_offset(x), doremir_event_offset(y));
    }

    case switch_event: {
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        return doremir_min(doremir_event_offset(x), doremir_event_offset(y));
    }

    case send_event: {
        event_t x = send_get(event, event);
        return doremir_event_offset(x);
    }

    case recv_event: {
        return TIME_ZERO;
    }

    default:
        assert(false && "Missing label");
    }
}

bool doremir_event_is_never(doremir_event_t event)
{
    // isNever never             = True
    // isNever now               = False
    // isNever (delay x t)       = isNever x
    // isNever (merge x y)       = isNever x && isNever y
    // isNever (switch p x y)    = if (isNever p) then (isNever x) else (isNever x && isNever y)
    // isNever (send x)          = isNever x
    // isNever (recv x)          = False

    switch (event->tag) {
    case never_event:
        return true;

    case now_event:
        return false;

    case delay_event: {
        event_t x = delay_get(event, event);
        return doremir_event_is_never(x);
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        return doremir_event_is_never(x) && doremir_event_is_never(y);
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);

        if (doremir_event_is_never(p)) {
            return doremir_event_is_never(x);
        } else {
            return doremir_event_is_never(x) && doremir_event_is_never(y);
        }
    }

    case send_event: {
        event_t x = send_get(event, event);
        return doremir_event_is_never(x);
    }

    case recv_event:
        return false;

    default:
        assert(false && "Missing label");
    }
}

void doremir_event_sync(doremir_event_t event)
{
    switch (event->tag) {
    case never_event:
        break;

    case now_event:
        break;

    case delay_event: {
        event_t x = delay_get(event, event);
        doremir_event_sync(x);
        return;
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        doremir_event_sync(x);
        doremir_event_sync(y);
        return;
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        doremir_event_sync(p);
        doremir_event_sync(x);
        doremir_event_sync(y);
        return;
    }

    case send_event: {
        event_t x = send_get(event, event);
        doremir_event_sync(x);
        return;
    }

    case recv_event: {
        sender_t s = recv_get(event, dispatcher);
        doremir_message_sync(s);              
        return;
    }
    
    }
}


bool doremir_event_has_value(doremir_time_t u, doremir_event_t event)
{
    // hasValue u never          = False
    // hasValue u (now x)        = True
    // hasValue u (delay t x)    = hasValue (u - t) x
    // hasValue u (merge x y)    = hasValue u x || hasValue u y
    // hasValue u (switch p x y) = if (!hasValue u p) then (hasValue u x) else (hasValue u y)
    // hasValue u (send d a x)   = hasValue u x
    // hasValue u (recv d a n)   = !isEmpty $ drop n (receive d a)

    switch (event->tag) {

    case never_event:
        return false;

    case now_event: {        
        return doremir_greater_than_equal(u, TIME_ZERO);
    }

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return doremir_event_has_value(doremir_subtract(u, t), x);
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        return doremir_event_has_value(u, x) || doremir_event_has_value(u, y);
        
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        return (!doremir_event_has_value(u, p)) 
            ? doremir_event_has_value(u, x) 
            : doremir_event_has_value(u, y);
    }

    case send_event: {
        event_t x = send_get(event, event);
        return doremir_event_has_value(u, x);
    }

    case recv_event: {
        sender_t s  = recv_get(event, dispatcher);
        sender_t a  = recv_get(event, address);
        list_t   xs = doremir_message_receive(s, a);
        return doremir_list_length(xs) > 0;
    }

    default:
        assert(false && "Missing label");
    }
}

doremir_ptr_t doremir_event_value(doremir_time_t u, doremir_event_t event)
{
    // value u never            = undefined
    // value u (now x)          = x
    // value u (delay t x)      = value (u - t) x
    // value u (merge x y)      = if (x < y && hasValue x) then (value x) else (value y)
    // value u (x `switch p` y) = if (!hasValue u p) then (value x) else (value y)
    // value u (send d a x)     = ()
    // value u (recv d a n)     = index n (receive d a)

    switch (event->tag) {

    case never_event: {
        assert(false && "Never has no value");
    }

    case now_event: {
        return now_get(event, value);
    }

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return doremir_event_value(doremir_subtract(u, t), x);
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        if (doremir_event_has_value(u, x))
            return doremir_event_value(u, x);
        else
            return doremir_event_value(u, y);
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        assert(false && "Not implemented");
    }

    case send_event: {
        receiver_t r = recv_get(event, dispatcher);
        sender_t   a = recv_get(event, address);
        event_t    x = send_get(event, event);
        ptr_t      vs = doremir_event_value(u, x);
        doremir_for_each(v, vs) {
            doremir_message_send(r, a, v);
        }
        return fb(false); // TODO something else for unit?
    }

    case recv_event: {
        sender_t s  = recv_get(event, dispatcher);
        sender_t a  = recv_get(event, address);
        return doremir_message_receive(s, a);
    }

    default:
        assert(false && "Missing label");
    }

}

/** Returns an event containing the first occurance.
 */
doremir_event_t doremir_event_head(doremir_event_t event)
{
    assert(false && "Not implemented.");
}

/** Returns an event containing all remaning occurances.
 */
doremir_event_t doremir_event_tail(doremir_time_t u, doremir_event_t event)
{
    // tail never            = undefined
    // tail now              = never
    // tail (delay t x)      = delay t (tail x)
    // tail (merge x y)      = if (x < y && hasValue x) then (tail x `merge` y) else (x `merge` tail y)
    // tail (x `switch p` y) = if (!hasValue p) then (tail x `switch p` y) else (tail y)
    // tail (send d a x)     = send d a (tail x)
    // tail (recv d a n)     = recv d a (n + 1)

// printf(">> Getting tail of: %s\n", unstring(doremir_string_show(event)));

    switch (event->tag) {

    case never_event:
        assert(false && "Never has no tail");

    case now_event:
        return doremir_event_never();

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return doremir_event_delay(t, doremir_event_tail(u, x));
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        if (doremir_event_has_value(u, x))
            return doremir_event_merge(doremir_event_tail(u, x), y);
        else if (doremir_event_has_value(u, y))
            return doremir_event_merge(x, doremir_event_tail(u, y));
        else
            return event;
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        assert(false && "Not implemented");
    }

    case send_event: {
        receiver_t r = recv_get(event, dispatcher);
        sender_t   a = recv_get(event, address);
        event_t    x = send_get(event, event);
        return doremir_event_send(r, a, doremir_event_tail(u, x));
    }

    case recv_event: {
        sender_t s  = recv_get(event, dispatcher);
        sender_t a  = recv_get(event, address);
        return doremir_event_receive(s, a);
    }

    default:
        assert(false && "Missing label");
    }
}





// --------------------------------------------------------------------------------

bool event_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    event_t c = (event_t) a;
    event_t d = (event_t) b;
    return doremir_equal(
               doremir_event_offset(c),
               doremir_event_offset(d));
    // assert(false && "No event equality");
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

    case now_event: {
        ptr_t value  = now_get(event, value);

        write_to(s, string("<Now "));
        write_to(s, doremir_string_show(value));
        write_to(s, string(">"));

        return s;
    }

    case send_event: {
        event_t x  = send_get(event, event);

        write_to(s, string("<Send "));
        write_to(s, doremir_string_show(x));
        write_to(s, string(">"));

        return s;
    }

    case recv_event:
        return string("<Receive>");

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x  = delay_get(event, event);

        write_to(s, string("<Delay "));
        write_to(s, doremir_string_show(t)); // FIXME
        write_to(s, string(" "));
        write_to(s, doremir_string_show(x));
        write_to(s, string(">"));

        return s;
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);

        write_to(s, string("<Merge "));
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

