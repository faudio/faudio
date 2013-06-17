
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/event.h>
#include <fae/set.h>
#include <fae/string.h>
#include <fae/message.h>
#include <fae/util.h>

// TODO rename, make public
// TODO max is arbitrary
#define TIME_ZERO fae_time_create(0,0,0,ratio(0,1))
#define TIME_MAX  fae_time_create(2000000,0,0,ratio(0,1))

#define event_inform(f,x)
// #define event_inform(f,x) fae_print(f,x)

/*
    Functional events built on primitives:
        never                    = []
        now x                    = [(0,x)]
        delay t as               = fmap (\(u,x) -> (t+u,x))
        merge as bs              = as `mergeBy fst` bs
        switch [] as bs          = as
        switch [(t:_),...] as bs =  filter ((u,x) -> (u < t,x)) as
                                    ++
                                    filter ((u,x) -> (u >= t,x)) as
        recv d a                 = ...
        send d a as              = ...

    Internal:
        hasValue []              = False
        hasTail  []              = False
        hasValue [(t,x):as]      = True
        hasTail  [(t,x):as]      = True

        offset   []              = Infinite
        offset   [(t,x):as]      = t
        value    [(t,x):as]      = x
        tail     [(t,x):as]      = as

    TODO
        - Recursive merge (with public fixpoint?)
        - Negative delay?
        - Proper delay of recv events
        - Map/filter/join
 */
struct _fae_event_t {

    impl_t      impl;

    enum {
        never_event,      // E a
        now_event,        // a -> E a
        delay_event,      // t -> E a -> E a
        merge_event,      // E a -> E a -> E a
        switch_event,     // E a -> E b -> E b -> E b
        send_event,       // D x a -> x -> E a -> E ()
        recv_event,       // D x a -> x -> E a
        filter_event,
        map_event
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
            list_t          history;
        }                   recv;
        struct {
            pred_t          pred;
            ptr_t           data;
            ptr_t           event;
        }                   filter;
        struct {
            unary_t         func;
            ptr_t           data;
            ptr_t           event;
        }                   map;

    }                       fields;
};


// --------------------------------------------------------------------------------

fae_event_t new_event(int tag)
{
    event_t e = fae_new(event);
    fae_ptr_t event_impl(fae_id_t interface);

    e->impl = &event_impl;
    e->tag  = tag;

    return e;
}

void delete_event(fae_event_t event)
{
    fae_delete(event);
}

#define is_never(v)       (v->tag == never_event)
#define is_now(v)         (v->tag == now_event)
#define is_delay(v)       (v->tag == delay_event)
#define is_merge(v)       (v->tag == merge_event)
#define is_switch(v)      (v->tag == switch_event)
#define is_send(v)        (v->tag == send_event)
#define is_recv(v)        (v->tag == recv_event)
#define is_filter(v)      (v->tag == filter_event)
#define is_map(v)         (v->tag == map_event)

#define now_get(v,f)      (v->fields.now.f)
#define delay_get(v,f)    (v->fields.delay.f)
#define merge_get(v,f)    (v->fields.merge.f)
#define switch_get(v,f)   (v->fields.switch_.f)
#define send_get(v,f)     (v->fields.send.f)
#define recv_get(v,f)     (v->fields.recv.f)
#define filter_get(v,f)   (v->fields.filter.f)
#define map_get(v,f)      (v->fields.map.f)

// --------------------------------------------------------------------------------

#pragma mark -

/** Create an empty event.
    This event never occurs.
    @return         A new event.
 */
fae_event_t fae_event_never()
{
    event_t e = new_event(never_event);
    return e;
}

/** Create a single event, occuring directly.
    @return         A new event.
 */
fae_event_t fae_event_now(fae_ptr_t value)
{
    event_t e = new_event(now_event);
    now_get(e, value) = value;
    return e;
}

/** Create a single event, occuring after the given time has passed.
    @return         A new event.
 */
fae_event_t fae_event_later(fae_time_t u, fae_ptr_t x)
{
    return fae_event_delay(u, fae_event_now(x));
}

/** Delay an event by the given amount of time.

    @param time     Amount of time to delay.
    @param event    Event to delay.
    @return         A new event.

    @par Laws

        delay t (delay u)   = delay (t + u)
        delay t (merge x y) = delay t x `merge` delay t u

 */
fae_event_t fae_event_delay(fae_time_t  time,
                                    fae_event_t event)
{

    assert(event && "Can not delay null");

    // if (is_never(event)) {
    //     return never();
    // }
    //
    // if (is_delay(event)) {
    //     time_t  t = delay_get(event, time);
    //     event_t x = delay_get(event, event);
    //     return fae_event_delay(fae_add(time, t), x);
    // }

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
fae_event_t fae_event_merge(fae_event_t event1,
                                    fae_event_t event2)
{
    assert(event1 && "Can not merge null");
    assert(event2 && "Can not merge null");

    // if (is_never(event1)) {
    //     return event2;
    // }
    //
    // if (is_never(event2)) {
    //     return event1;
    // }

    // invariant left <= right
    event_t e = new_event(merge_event);

    if (fae_less_than_equal(event1, event2)) {
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
fae_event_t fae_event_switch(fae_event_t pred,
                                     fae_event_t event1,
                                     fae_event_t event2)
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
fae_event_t fae_event_send(fae_message_receiver_t   receiver,
                                   fae_message_address_t    address,
                                   fae_event_t              event)
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
fae_event_t fae_event_receive(fae_message_sender_t  sender,
                                      fae_message_address_t address)
{
    assert(sender  && "Need a sender");

    event_t e = new_event(recv_event);
    recv_get(e, dispatcher) = sender;
    recv_get(e, address)    = address;
    recv_get(e, history)    = fae_list_empty();
    return e;
}

/** Destroy the given event.
 */
void fae_event_destroy(fae_event_t event)
{
    delete_event(event);
}

fae_event_t fae_event_filter(fae_pred_t  pred,
                                     fae_ptr_t   data,
                                     fae_event_t event)
{
    assert(event     && "Need an event");

    event_t e = new_event(filter_event);
    filter_get(e, pred)   = pred;
    filter_get(e, data)   = data;
    filter_get(e, event)  = event;
    return e;

}

fae_event_t fae_event_map(fae_unary_t    func,
                                  fae_ptr_t      data,
                                  fae_event_t    event)
{
    assert(event     && "Need an event");

    event_t e = new_event(map_event);
    map_get(e, func)   = func;
    map_get(e, data)   = data;
    map_get(e, event)  = event;
    return e;
}

fae_event_t fae_event_map2(fae_binary_t func,
                                   fae_ptr_t    data,
                                   fae_event_t  event1,
                                   fae_event_t  event2)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

#pragma mark -

/** Return the minumum offset to the first occurence.

    Note that the event may or may not have an actual occurence at this time.
 */
fae_time_t fae_event_offset(fae_event_t event)
{
    switch (event->tag) {

    case never_event:
        return TIME_MAX;

    case now_event:
        return TIME_ZERO;

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return fae_add(fae_event_offset(x), t);
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        return fae_min(fae_event_offset(x), fae_event_offset(y));
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        return fae_min(fae_event_offset(p),
                           fae_min(fae_event_offset(x), fae_event_offset(y)));
    }

    case send_event: {
        event_t x = send_get(event, event);
        return fae_event_offset(x);
    }

    case recv_event: {
        return TIME_ZERO;
    }

    case filter_event: {
        event_t x = filter_get(event, event);
        return fae_event_offset(x);
    }

    case map_event: {
        event_t x = map_get(event, event);
        return fae_event_offset(x);
    }

    default:
        assert(false && "Missing label");
    }
}

void fae_event_add_sync(
    void (*func)(fae_ptr_t, fae_message_sender_t),
    fae_ptr_t     data,
    fae_event_t   event
)
{
    switch (event->tag) {
    case never_event:
        break;

    case now_event:
        break;

    case delay_event: {
        event_t x = delay_get(event, event);
        fae_event_add_sync(func, data, x);
        return;
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        fae_event_add_sync(func, data, x);
        fae_event_add_sync(func, data, y);
        return;
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        fae_event_add_sync(func, data, p);
        fae_event_add_sync(func, data, x);
        fae_event_add_sync(func, data, y);
        return;
    }

    case send_event: {
        event_t x = send_get(event, event);
        fae_event_add_sync(func, data, x);
        return;
    }

    case recv_event: {
        sender_t s = recv_get(event, dispatcher);
        func(data, s);
        return;
    }

    case filter_event: {
        event_t x = filter_get(event, event);
        fae_event_add_sync(func, data, x);
        return;
    }

    case map_event: {
        event_t x = map_get(event, event);
        fae_event_add_sync(func, data, x);
        return;
    }

    default:
        assert(false && "Missing label");
    }
}

inline static list_t never_values(time_t begin, time_t end, event_t event)
{
    return fae_list_empty();
}

inline static list_t now_values(time_t begin, time_t end, event_t event)
{
    // begin <= t < end
    if (fae_less_than_equal(begin, TIME_ZERO) && fae_less_than(TIME_ZERO, end)) {
        return fae_list_single(now_get(event, value));
    } else {
        return fae_list_empty();
    }
}

inline static list_t delay_values(time_t begin, time_t end, event_t event)
{
    time_t  t = delay_get(event, time);
    event_t x = delay_get(event, event);
    return fae_event_values(fae_subtract(begin, t), fae_subtract(end, t), x);
}

inline static list_t merge_values(time_t begin, time_t end, event_t event)
{
    event_t x = merge_get(event, left);
    event_t y = merge_get(event, right);

    return fae_list_dappend(fae_event_values(begin, end, x),
                                fae_event_values(begin, end, y));
}

inline static list_t switch_values(time_t begin, time_t end, event_t event)
{
    event_t p = switch_get(event, pred);
    event_t x = switch_get(event, before);
    event_t y = switch_get(event, after);

    if (!fae_event_has_values(TIME_ZERO, end, p)) {
        // event_inform("\x1b[34mChoosing left switch tail\n\x1b[0m", begin);
        return fae_event_values(begin, end, x);
    } else {
        // event_inform("\x1b[34mChoosing right switch tail\n\x1b[0m", begin);
        return fae_event_values(begin, end, y);
    }
}

inline static list_t send_values(time_t begin, time_t end, event_t event)
{
    receiver_t r  = send_get(event, dispatcher);
    sender_t   a  = send_get(event, address);
    event_t    x  = send_get(event, event);
    ptr_t      vs = fae_event_values(begin, end, x);

    fae_for_each(v, vs) {
        fae_message_send(r, a, v);
    }
    return fb(true); // TODO something else for unit?
}

inline static list_t recv_values(time_t begin, time_t end, event_t event)
{
    /*  TODO

        This is a bit of a hack. If begin == 0, return entire history,
        otherwise return the current input. We ought to return a slice of history.

        Semantically, recv can look at an arbitrary time into the past. In reality
        we do not want to cache all inputs in memory. The problem is that while
        both delay and switch depend on history, delay should depend on bounded
        history, and switch on full history (and only whether it is empty or not).

        We should probably change the switch primitive to use some specialized
        version of has_values referencing a bool field in the recv event rather
        than comparing against history. Then we could store history in a dynamic
        array instead of a list.
     */
    sender_t s  = recv_get(event, dispatcher);
    sender_t a  = recv_get(event, address);
    list_t   history  = recv_get(event, history);
    list_t   current  = fae_message_receive(s, a);

    history = fae_list_dappend(history, fae_list_copy(current));
    recv_get(event, history) = history;

    if (fae_equal(begin, TIME_ZERO)) {
        return history;
    } else {
        return current;
    }
}

inline static list_t filter_values(time_t begin, time_t end, event_t event)
{
    pred_t     pred  = filter_get(event, pred);
    ptr_t      data  = filter_get(event, data);
    event_t    x     = filter_get(event, event);
    return fae_list_filter(pred, data, fae_event_values(begin, end, x));
}

inline static list_t map_values(time_t begin, time_t end, event_t event)
{
    unary_t    func  = map_get(event, func);
    ptr_t      data  = map_get(event, data);
    event_t    x     = map_get(event, event);
    return fae_list_map(func, data, fae_event_values(begin, end, x));
}


fae_list_t fae_event_values(fae_time_t  begin,
                                    fae_time_t  end,
                                    fae_event_t event)
{
    switch (event->tag) {
    case never_event:
        return never_values(begin, end, event);

    case now_event:
        return now_values(begin, end, event);

    case delay_event:
        return delay_values(begin, end, event);

    case merge_event:
        return merge_values(begin, end, event);

    case switch_event:
        return switch_values(begin, end, event);

    case send_event:
        return send_values(begin, end, event);

    case recv_event:
        return recv_values(begin, end, event);

    case filter_event:
        return filter_values(begin, end, event);

    case map_event:
        return map_values(begin, end, event);

    default:
        assert(false && "Missing label");
    }
}

bool fae_event_has_values(fae_time_t    begin,
                              fae_time_t    end,
                              fae_event_t   event)
{
    return !fae_list_is_empty(fae_event_values(begin, end, event));
}

bool fae_event_has_more(fae_time_t      at,
                            fae_event_t     event)
{
    switch (event->tag) {

    case never_event:
        return false;

    case now_event:
        return fae_less_than_equal(at, TIME_ZERO);

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x = delay_get(event, event);
        return fae_event_has_more(fae_subtract(at, t), x);
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);
        return fae_event_has_more(at, x) || fae_event_has_more(at, y);
    }

    case switch_event: {
        // event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);
        // TODO use p to optimize?
        return fae_event_has_more(at, x) || fae_event_has_more(at, y);
    }

    case send_event: {
        event_t x = send_get(event, event);
        return fae_event_has_more(at, x);
    }

    case recv_event: {
        return true;
    }

    case filter_event: {
        event_t x = filter_get(event, event);
        return fae_event_has_more(at, x);
    }

    case map_event: {
        event_t x = map_get(event, event);
        return fae_event_has_more(at, x);
    }

    default:
        assert(false && "Missing label");
    }
}




// --------------------------------------------------------------------------------

#pragma mark -

// first x              = before (tail x) x
// after p x           = switch p never x
// before p x          = switch p x never

// FIXME
fae_event_t fae_event_after(fae_event_t p, fae_event_t x)
{
    return fae_event_switch(p, never(), x);
}

fae_event_t fae_event_before(fae_event_t p, fae_event_t x)
{
    return fae_event_switch(p, x, never());
}





// --------------------------------------------------------------------------------

#pragma mark -

bool event_equal(fae_ptr_t a, fae_ptr_t b)
{
    event_t c = (event_t) a;
    event_t d = (event_t) b;
    return fae_equal(
               fae_event_offset(c),
               fae_event_offset(d));
}

bool event_less_than(fae_ptr_t a, fae_ptr_t b)
{
    event_t c = (event_t) a;
    event_t d = (event_t) b;
    return fae_less_than(
               fae_event_offset(c),
               fae_event_offset(d));
}

bool event_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    return event_less_than(b, a);
}

inline static string_t print_event(int n, event_t a)
{
    event_t event = (event_t) a;
    string_t s = string("");
    string_t ident = fae_string_repeat(n * 2, ' ');

    switch (event->tag) {

    case never_event:
        return string("(Never)");

    case now_event: {
        ptr_t value  = now_get(event, value);

        write_to(s, string("(Now "));
        write_to(s, fae_string_show(value));
        write_to(s, string(")"));

        return s;
    }

    case delay_event: {
        time_t  t = delay_get(event, time);
        event_t x  = delay_get(event, event);

        write_to(s, string("(Delay "));
        write_to(s, fae_string_show(t)); // FIXME
        write_to(s, string(" \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(")"));

        return s;
    }

    case merge_event: {
        event_t x = merge_get(event, left);
        event_t y = merge_get(event, right);

        write_to(s, string("(Merge \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(" \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, y));
        write_to(s, string(")"));

        return s;
    }

    case switch_event: {
        event_t p = switch_get(event, pred);
        event_t x = switch_get(event, before);
        event_t y = switch_get(event, after);

        write_to(s, string("(Switch \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, p));
        write_to(s, string(" \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(" \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, y));
        write_to(s, string(")"));

        return s;
    }

    case send_event: {
        event_t x  = send_get(event, event);

        write_to(s, string("(Send \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(")"));

        return s;
    }

    case recv_event:
        return string("(Receive)");

    case map_event: {
        event_t x  = map_get(event, event);

        write_to(s, string("(Map \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(")"));

        return s;
    }

    case filter_event: {
        event_t x  = filter_get(event, event);

        write_to(s, string("(Filter \n"));
        write_to(s, fae_string_copy(ident));
        write_to(s, print_event(n + 1, x));
        write_to(s, string(")"));

        return s;
    }

    default:
        assert(false && "Missing label");
    }
}

string_t event_show(fae_ptr_t a)
{
    return print_event(1, a);
}

void event_destroy(fae_ptr_t a)
{
    fae_event_destroy(a);
}

fae_ptr_t event_impl(fae_id_t interface)
{
    static fae_equal_t event_equal_impl = { event_equal };
    static fae_order_t event_order_impl = { event_less_than, event_greater_than };
    static fae_string_show_t event_show_impl = { event_show };
    static fae_destroy_t event_destroy_impl = { event_destroy };

    switch (interface) {
    case fae_equal_i:
        return &event_equal_impl;

    case fae_order_i:
        return &event_order_impl;

    case fae_string_show_i:
        return &event_show_impl;

    case fae_destroy_i:
        return &event_destroy_impl;

    default:
        return NULL;
    }
}

