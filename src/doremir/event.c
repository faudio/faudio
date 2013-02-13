
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/event.h>
#include <doremir/set.h>
#include <doremir/string.h>
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
            int             index;          // Offset in incoming elements
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
    This event never occur.
    @return         A new event.
 */
doremir_event_t doremir_event_never()
{
    event_t e = new_event(never_event);
    return e;
}

/** Create a single event.
    This event occurs exactly once at scheduling time.
    @return         A new event.
 */
doremir_event_t doremir_event_now(doremir_ptr_t value)
{
    event_t e = new_event(now_event);
    now_get(e, value) = value;
    return e;
}

/** Delay an event by the given amount of time.
    @param time     Amount of time to delay.
    @param event2   Event to delay.
    @return         A new event.
 */
doremir_event_t doremir_event_delay(doremir_time_t time,
                                    doremir_event_t event)
{
    // delay t (merge x y) = delay t x `merge` delay t u
    // delay t (delay u)   = delay (t + u)

    assert(event && "Can not delay null");

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
 */
doremir_event_t doremir_event_merge(doremir_event_t event1,
                                    doremir_event_t event2)
{
    // merge (switch p x y) (switch p y x) = merge x y
    // merge (merge x y) z = merge x (merge y z)
    // merge x never       = x
    // merge x y           = merge y x
    assert(event1 && "Can not merge null");
    assert(event2 && "Can not merge null");

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

/** Create an event that switches from one event to another at a given time.
    @param trigger  Triggering event.
    @param event1   Behave as this event before the trigger occurs.
    @param event2   Behave as this event after the trigger occurs.
    @return         A new event.
 */
doremir_event_t doremir_event_switch(doremir_event_t pred,
                                     doremir_event_t event1,
                                     doremir_event_t event2)
{
    // switch x never x    = x
    // switch never x y    = x
    // switch x x never    = never
    // switch (one _) x y  = y

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

static inline event_t receive2(
    doremir_message_sender_t  sender,
    doremir_message_address_t address,
    int index)
{
    assert(sender  && "Need a sender");

    event_t e = new_event(recv_event);
    recv_get(e, dispatcher) = sender;
    recv_get(e, address)    = address;
    recv_get(e, index)      = index;
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
    return receive2(sender, address, 0);
}


void doremir_event_destroy(doremir_event_t event)
{
    // TODO manage components
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
        event_t x = delay_get(event, event);
        time_t  t = delay_get(event, time);
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
    // TODO distribute
    return is_never(event);
}

void doremir_event_sync(doremir_event_t event)
{
    switch (event->tag) {
    case never_event:
        break;

    case now_event:
        break;

    case delay_event:
        doremir_event_sync(delay_get(event, event));
        break;

    case merge_event:
        doremir_event_sync(merge_get(event, left));
        doremir_event_sync(merge_get(event, right));
        break;

    case switch_event:
        doremir_event_sync(switch_get(event, pred));
        doremir_event_sync(switch_get(event, before));
        doremir_event_sync(switch_get(event, after));
        break;

    case send_event:
        doremir_event_sync(send_get(event, event));
        break;

    case recv_event:
        doremir_message_sync(recv_get(event, dispatcher));
        // FIXME mutate index?
        break;
    }
}


bool doremir_event_has_value(doremir_time_t time, doremir_event_t event)
{
    // hasValue u never          = False
    // hasValue u (now x)        = True
    // hasValue u (delay t x)    = hasValue (u - t) x
    // hasValue u (merge x y)    = hasValue u x || hasValue u y
    // hasValue u (switch p x y) = if (!hasValue u p) then (hasValue u x) else (hasValue u y)
    // hasValue u (recv d a n)   = !isEmpty $ drop n (receive d a)
    // hasValue u (send d a x)   = hasValue u x

    switch (event->tag) {

    case never_event:
        return false;

    case now_event:
        return doremir_greater_than_equal(time, TIME_ZERO);

    case delay_event:
        return doremir_event_has_value(
                   doremir_subtract(time, delay_get(event, time)),
                   delay_get(event, event)
               );

    case merge_event:
        return doremir_event_has_value(time, merge_get(event, left))
               || doremir_event_has_value(time, merge_get(event, right));

    case switch_event:
        return (!doremir_event_has_value(time, switch_get(event, pred)))
               ? doremir_event_has_value(time, switch_get(event, before))
               : doremir_event_has_value(time, switch_get(event, after));

    case send_event: {
        return doremir_event_has_value(time, send_get(event, event));
    }

    case recv_event: {
        return !doremir_list_is_empty(
                   doremir_list_drop(recv_get(event, index),
                                     doremir_message_receive(
                                         recv_get(event, dispatcher),
                                         recv_get(event, address))));
    }

    default:
        assert(false && "Missing label");
    }
}

doremir_ptr_t doremir_event_value(doremir_event_t event)
{
    // value u never            = undefined
    // value u (now x)          = x
    // value u (delay t x)      = value x
    // value u (merge x y)      = if (x < y) then (value x) else (value y)
    // value u (x `switch p` y) = if (!hasValue u p) then (value x) else (value y)
    // value u (recv d a n)     = index n (receive d a)
    // value u (send d a x)     = ()

    switch (event->tag) {

    case never_event:
        assert(false && "Never has no value");

    case now_event:
        return now_get(event, value);

    case delay_event:
        return doremir_event_value(delay_get(event, event));

    case merge_event:
        return doremir_event_value(merge_get(event, left));

    case switch_event:
        assert(false && "Not implemented");

    case send_event: {
        ptr_t value = doremir_event_value(send_get(event, event));
        doremir_message_send(
            send_get(event, dispatcher),
            send_get(event, address),
            value);

        return fb(false);
    }

    case recv_event: {
        // doremir_message_sync(recv_get(event, dispatcher)); // FIXME
        return doremir_list_index(recv_get(event, index), doremir_message_receive(
                                      recv_get(event, dispatcher),
                                      recv_get(event, address)));
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
doremir_event_t doremir_event_tail(doremir_event_t event)
{
    // tail never            = undefined
    // tail now              = never
    // tail (delay t x)      = delay t (tail x)
    // tail (merge x y)      = if (x < y) then (tail x `merge` y) else (x `merge` tail y)
    // tail (x `switch p` y) = if (!hasValue p) then (tail x `switch p` y) else (tail y)
    // tail (recv d a n)     = recv d a (n + 1)
    // tail (send d a x)     = send d a (tail x)

// printf(">> Getting tail of: %s\n", unstring(doremir_string_show(event)));

    switch (event->tag) {

    case never_event:
        assert(false && "Never has no tail");

    case now_event:
        return doremir_event_never();

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return doremir_event_delay(t, doremir_event_tail(x));
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        return doremir_event_merge(doremir_event_tail(x), y);
    }

    case switch_event: {
        assert(false && "Not implemented");
        // event_t p = switch_get(event, pred);
        // event_t tx = doremir_event_tail(switch_get(event, before));
        // event_t ty = doremir_event_tail(switch_get(event, after));
        // return doremir_event_switch(p, tx, ty);
    }

    case send_event: {
        event_t x = send_get(event, event);
        return doremir_event_send(
                   send_get(event, dispatcher),
                   send_get(event, address),
                   doremir_event_tail(x));
    }

    case recv_event: {
        return receive2(
                   recv_get(event, dispatcher),
                   recv_get(event, address),
                   recv_get(event, index) + 1);
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
        write_to(s, format_integer("%d", doremir_time_seconds(t))); // FIXME
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

