
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

#define event_inform(f,x)
// #define event_inform(f,x) doremir_print(f,x)

/*
    TODO
        - Recursive merge (with public fixpoint?)
        - Switch
        - Map/filter/join
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

#pragma mark -

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

    The resulting event has all occurences of the given events interleaved.

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

    if (is_never(event1)) {
        return event2;
    }

    if (is_never(event2)) {
        return event1;
    }

    // invariant left <= right
    event_t e = new_event(merge_event);

    if (doremir_less_than_equal(event1, event2)) {
        merge_get(e, left)   = event1;
        merge_get(e, right)  = event2;
    } else {
        merge_get(e, left)   = event2;
        merge_get(e, right)  = event1;
    }

    return e;
}

/** Create an event that switches from one event to another at a the first
    occurence of a trigger event.

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


// --------------------------------------------------------------------------------

#pragma mark -

/** Return the minumum offset to the first occurence.

    Note that the event may or may not have an actual occurence at this time.
 */
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

/** Syncronize this event with the external world. In particular, this function
    makes all receive events syncronize with their sender.

    When used in a scheduler context, this function should be called once per
    scheduler iteration.
 */
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

    default:
        assert(false && "Missing label");
    }
}

void doremir_event_add_sync(void (*func)(doremir_ptr_t,
                                         doremir_message_sender_t),
                            doremir_ptr_t data,
                            doremir_event_t event)
{
    switch (event->tag) {
    case never_event:
        break;

    case now_event:
        break;

    case delay_event: {
        event_t x = delay_get(event, event);
        doremir_event_add_sync(func, data, x);
        return;
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        doremir_event_add_sync(func, data, x);
        doremir_event_add_sync(func, data, y);
        return;
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        doremir_event_add_sync(func, data, p);
        doremir_event_add_sync(func, data, x);
        doremir_event_add_sync(func, data, y);
        return;
    }

    case send_event: {
        event_t x = send_get(event, event);
        doremir_event_add_sync(func, data, x);
        return;
    }

    case recv_event: {
        sender_t s = recv_get(event, dispatcher);
        func(data, s);
        return;
    }

    default:
        assert(false && "Missing label");
    }

}



#define has_occured doremir_event_has_value


bool never_has_value(doremir_time_t current, doremir_event_t event)
{
    return false;
}
bool never_has_tail(doremir_time_t current, doremir_event_t event)
{
    return false;
}
ptr_t never_value(doremir_time_t current, doremir_event_t event)
{
    assert(false && "Impossible");
}
event_t never_tail(doremir_time_t current, doremir_event_t event)
{
    assert(false && "Impossible");
}

bool now_has_value(doremir_time_t current, doremir_event_t event)
{
    return doremir_greater_than_equal(current, TIME_ZERO);
}
bool now_has_tail(doremir_time_t current, doremir_event_t event)
{
    return true;
}
ptr_t now_value(doremir_time_t current, doremir_event_t event)
{
    return now_get(event, value);
}
event_t now_tail(doremir_time_t current, doremir_event_t event)
{
    return never();
}

bool delay_has_value(doremir_time_t current, doremir_event_t event)
{
    time_t  t = delay_get(event, time);
    event_t x = delay_get(event, event);

    return doremir_event_has_value(doremir_subtract(current, t), x);
}
bool delay_has_tail(doremir_time_t current, doremir_event_t event)
{
    time_t  t = delay_get(event, time);
    event_t x = delay_get(event, event);

    return doremir_event_has_tail(doremir_subtract(current, t), x);
}
ptr_t delay_value(doremir_time_t current, doremir_event_t event)
{
    time_t  t = delay_get(event, time);
    event_t x = delay_get(event, event);

    return doremir_event_value(doremir_subtract(current, t), x);
}
event_t delay_tail(doremir_time_t current, doremir_event_t event)
{
    time_t  t = delay_get(event, time);
    event_t x = delay_get(event, event);
    return doremir_event_delay(t, doremir_event_tail(doremir_subtract(current, t), x));
}

bool merge_has_value(doremir_time_t current, doremir_event_t event)
{
    event_t x = merge_get(event, left);
    event_t y = merge_get(event, right);

    return doremir_event_has_value(current, x) || doremir_event_has_value(current, y);
}
bool merge_has_tail(doremir_time_t current, doremir_event_t event)
{
    event_t x = merge_get(event, left);
    event_t y = merge_get(event, right);
// event_inform("Checking merge tail at: %s\n", current);

    return doremir_event_has_tail(current, x) || doremir_event_has_tail(current, y);
}
ptr_t merge_value(doremir_time_t current, doremir_event_t event)
{
    event_t x = merge_get(event, left);
    event_t y = merge_get(event, right);

    if (doremir_event_has_value(current, x)) {
        event_inform("\x1b[34mChoosing left merge value\n\x1b[0m", current);
        return doremir_event_value(current, x);
    }

    if (doremir_event_has_value(current, y)) {
        event_inform("\x1b[34mChoosing right merge value\n\x1b[0m", current);
        return doremir_event_value(current, y);
    }

    assert(false && "Impossible, merge(never,never) has no values");
}
event_t merge_tail(doremir_time_t current, doremir_event_t event)
{
    event_t x = merge_get(event, left);
    event_t y = merge_get(event, right);
// event_inform("Getting merge tail at: %s\n", current);

    if (doremir_event_has_value(current, x)) {
        event_inform("\x1b[34mChoosing left merge tail\n\x1b[0m", current);

        if (doremir_event_has_tail(current, x)) {
            return doremir_event_merge(doremir_event_tail(current, x), y);
        } else {
            return doremir_event_tail(current, y);
        }
    }

    if (doremir_event_has_value(current, y)) {
        event_inform("\x1b[34mChoosing right merge tail\n\x1b[0m", current);

        if (doremir_event_has_tail(current, y)) {
            return doremir_event_merge(x, doremir_event_tail(current, y));
        } else {
            return doremir_event_tail(current, x);
        }
    }

    if (doremir_event_has_tail(current, x)) {
        return doremir_event_merge(doremir_event_tail(current, x), y);
    }

    if (doremir_event_has_tail(current, y)) {
        return doremir_event_merge(x, doremir_event_tail(current, y));
    }

    return event;
}

bool switch_has_value(doremir_time_t current, doremir_event_t event)
{
    event_t p = switch_get(event, pred);
    event_t x = switch_get(event, before);
    event_t y = switch_get(event, after);
    event_inform("\x1b[31mChecking switch value at %s\n\x1b[0m", current);
// printf("    Occured: %d\n", has_occured(current, p));

    if (!has_occured(current, p)) {
        return doremir_event_has_value(current, x);
    } else {
        return doremir_event_has_value(current, y);
    }
}
bool switch_has_tail(doremir_time_t current, doremir_event_t event)
{
    event_t p = switch_get(event, pred);
    event_t x = switch_get(event, before);
    event_t y = switch_get(event, after);

    if (!has_occured(current, p)) {
        return (doremir_event_has_tail(current, x) || doremir_event_has_tail(current, y));
    } else {
        return doremir_event_has_tail(current, y);
    }
}
ptr_t switch_value(doremir_time_t current, doremir_event_t event)
{
    event_t p = switch_get(event, pred);
    event_t x = switch_get(event, before);
    event_t y = switch_get(event, after);
    event_inform("\x1b[32mGetting switch value at %s\n\x1b[0m", current);

    if (!has_occured(current, p)) {
        return doremir_event_value(current, x);
    } else {
        return doremir_event_value(current, y);
    }
}
event_t switch_tail(doremir_time_t current, doremir_event_t event)
{
    event_t p = switch_get(event, pred);
    event_t x = switch_get(event, before);
    event_t y = switch_get(event, after);
    event_inform("\x1b[33mGetting switch tail at %s\n\x1b[0m", current);

    if (!has_occured(current, p)) {
        event_inform("\x1b[34mChoosing left switch tail\n\x1b[0m", current);

        if (!doremir_event_has_value(current, x)) {
            return event;
        }

        if (doremir_event_has_tail(current, x)) {
            return doremir_event_switch(p, doremir_event_tail(current, x), y);
        } else {
            return doremir_event_switch(p, never(), y);
        }
    } else {
        event_inform("\x1b[34mChoosing right switch tail\n\x1b[0m", current);
        return doremir_event_tail(current, y);
    }
}

bool send_has_value(doremir_time_t current, doremir_event_t event)
{
    event_t x = send_get(event, event);
    return doremir_event_has_value(current, x);
}
bool send_has_tail(doremir_time_t current, doremir_event_t event)
{
    event_t x = send_get(event, event);
    return doremir_event_has_tail(current, x);
}
ptr_t send_value(doremir_time_t current, doremir_event_t event)
{
    receiver_t r = recv_get(event, dispatcher);
    sender_t   a = recv_get(event, address);
    event_t    x = send_get(event, event);
    ptr_t      vs = doremir_event_value(current, x);

    doremir_for_each(v, vs) {
        doremir_message_send(r, a, v);
    }
    return fb(true); // TODO something else for unit?
}
event_t send_tail(doremir_time_t current, doremir_event_t event)
{
    receiver_t r = recv_get(event, dispatcher);
    sender_t   a = recv_get(event, address);
    event_t    x = send_get(event, event);

    return doremir_event_send(r, a, doremir_event_tail(current, x));
}

bool recv_has_value(doremir_time_t current, doremir_event_t event)
{
    sender_t s  = recv_get(event, dispatcher);
    sender_t a  = recv_get(event, address);
    list_t   xs = doremir_message_receive(s, a);
    return doremir_list_length(xs) > 0;
}
bool recv_has_tail(doremir_time_t current, doremir_event_t event)
{
    return true;
}
ptr_t recv_value(doremir_time_t current, doremir_event_t event)
{
    sender_t s  = recv_get(event, dispatcher);
    sender_t a  = recv_get(event, address);
    return doremir_message_receive(s, a);
}
event_t recv_tail(doremir_time_t current, doremir_event_t event)
{
    // Add infinitesimal offset
    return event;
}




bool doremir_event_has_value(doremir_time_t current, doremir_event_t event)
{
    switch (event->tag) {

    case never_event:
        return never_has_value(current, event);

    case now_event:
        return now_has_value(current, event);

    case delay_event:
        return delay_has_value(current, event);

    case merge_event:
        return merge_has_value(current, event);

    case switch_event:
        return switch_has_value(current, event);

    case send_event:
        return send_has_value(current, event);

    case recv_event:
        return recv_has_value(current, event);

    default:
        assert(false && "Missing label");
    }
}

bool doremir_event_has_tail(doremir_time_t current, doremir_event_t event)
{
    switch (event->tag) {
    case never_event:
        return never_has_tail(current, event);

    case now_event:
        return now_has_tail(current, event);

    case delay_event:
        return delay_has_tail(current, event);

    case merge_event:
        return merge_has_tail(current, event);

    case switch_event:
        return switch_has_tail(current, event);

    case send_event:
        return send_has_tail(current, event);

    case recv_event:
        return recv_has_tail(current, event);

    default:
        assert(false && "Missing label");
    }
}
// bool doremir_event_has_tail(doremir_time_t u, doremir_event_t event)
// {
//     bool res = doremir_event_has_tail2(u, event);
//     // event_inform("tail(%s)", event);
//     // event_inform(" ==> %s\n", fb(res));
//     return res;
// }

doremir_ptr_t doremir_event_value(doremir_time_t current, doremir_event_t event)
{
    assert(doremir_event_has_value(current, event) && "No value");

    switch (event->tag) {
    case never_event:
        return never_value(current, event);

    case now_event:
        return now_value(current, event);

    case delay_event:
        return delay_value(current, event);

    case merge_event:
        return merge_value(current, event);

    case switch_event:
        return switch_value(current, event);

    case send_event:
        return send_value(current, event);

    case recv_event:
        return recv_value(current, event);

    default:
        assert(false && "Missing label");
    }
}

/** Returns an event containing all remaning occurences.
 */
doremir_event_t doremir_event_tail(doremir_time_t current, doremir_event_t event)
{
    assert(doremir_event_has_tail(current, event) && "No tail");

    switch (event->tag) {
    case never_event:
        return never_tail(current, event);

    case now_event:
        return now_tail(current, event);

    case delay_event:
        return delay_tail(current, event);

    case merge_event:
        return merge_tail(current, event);

    case switch_event:
        return switch_tail(current, event);

    case send_event:
        return send_tail(current, event);

    case recv_event:
        return recv_tail(current, event);

    default:
        assert(false && "Missing label");
    }
}



// --------------------------------------------------------------------------------

#pragma mark -

// head x              = before (tail x) x
// after p x           = switch p never x
// before p x          = switch p x never

doremir_event_t doremir_event_head(doremir_event_t x)
{
    // assert(false && "Not implemented");
    // time_t t = doremir_event_offset(x);
    return doremir_event_before(doremir_event_tail(TIME_MAX, x), x);
}

// FIXME
doremir_event_t doremir_event_after(doremir_event_t p, doremir_event_t x)
{
    return doremir_event_switch(p, never(), x);
}

doremir_event_t doremir_event_before(doremir_event_t p, doremir_event_t x)
{
    return doremir_event_switch(p, x, never());
}





// --------------------------------------------------------------------------------

#pragma mark -

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

inline static string_t print_event(int n, event_t a)
{
    event_t event = (event_t) a;
    string_t s = string("");
    string_t ident = doremir_string_repeat(n * 2, ' ');

    switch (event->tag) {

    case never_event:
        return string("(Never)");

    case now_event: {
        ptr_t value  = now_get(event, value);

        write_to(s, string("(Now "));
        write_to(s, doremir_string_show(value));
        write_to(s, string(")"));

        return s;
    }

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x  = delay_get(event, event);

        write_to(s, string("(Delay "));
        write_to(s, doremir_string_show(t)); // FIXME
        write_to(s, string(" \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(")"));

        return s;
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);

        write_to(s, string("(Merge \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(" \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, y));
        write_to(s, string(")"));

        return s;
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);

        write_to(s, string("(Switch \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, p));
        write_to(s, string(" \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(" \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, y));
        write_to(s, string(")"));

        return s;
    }

    case send_event: {
        event_t x  = send_get(event, event);

        write_to(s, string("(Send \n"));
        write_to(s, doremir_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(")"));

        return s;
    }

    case recv_event:
        return string("(Receive)");

    default:
        assert(false && "Missing label");
    }
}

string_t event_show(doremir_ptr_t a)
{
    return print_event(1, a);
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

