
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/scheduler.h>
#include <doremir/priority_queue.h>
#include <doremir/util.h>

/*
    Notes:
 */

#define loop_interval_k 1

struct _doremir_scheduler_t {
    impl_t                  impl;           //  Dispatcher
    clock_t                 clock;          //  Provides time
    time_t                  start;          //  Start time, as per clock
    list_t                  senders;        //  Senders from which inputs are obtained
    priority_queue_t        queue;          //  Events to be evaluated
};


// -----------------------------------------------------------------------------

inline static scheduler_t new_scheduler(clock_t clock)
{
    ptr_t scheduler_impl(doremir_id_t interface);

    scheduler_t scheduler = doremir_new(scheduler);
    scheduler->impl     = &scheduler_impl;
    scheduler->clock    = clock;
    scheduler->start    = doremir_time_time(scheduler->clock);
    scheduler->senders  = doremir_list_empty();
    scheduler->queue    = priority_queue();
    return scheduler;
}

inline static void delete_scheduler(scheduler_t scheduler)
{
    doremir_delete(scheduler);
}


// -----------------------------------------------------------------------------

/** Schedule.
    @param clock            Must implement [Clock](@ref doremir_time_clock_t).
 */
doremir_scheduler_t doremir_scheduler_create(doremir_time_clock_t clock)
{
    return new_scheduler(clock);
}


void doremir_scheduler_destroy(scheduler_t scheduler)
{
    doremir_destroy(scheduler->queue);
    doremir_destroy(scheduler->start);
    delete_scheduler(scheduler);
}


static void collect_sync(doremir_ptr_t senders, doremir_message_sender_t sender)
{
    // Add hoc set to user pointer equality
    list_t *list = senders;
    doremir_for_each(s, *list) {
        if (s == sender) {
            return;
        }
    }
    *list = doremir_list_dcons(sender, *list);
}

/** Schedule.
    @param event            Event to schedule.
    @param scheduler        The scheduler.
 */
void doremir_scheduler_schedule(doremir_scheduler_t scheduler, doremir_event_t event)
{
    doremir_priority_queue_insert(event, scheduler->queue);
    doremir_event_add_sync(collect_sync, &scheduler->senders, event);
}

#define sched_inform(str)
// #define sched_inform(str) dinform(str)

#define max_events_k 500

void doremir_scheduler_execute(doremir_scheduler_t scheduler)
{
    time_t      abs_now = doremir_time_time(scheduler->clock);
    time_t      now     = doremir_subtract(abs_now, scheduler->start);
    event_t     recur[max_events_k];
    size_t      recurring = 0;

    sched_inform(string_dappend(string("@ "), doremir_string_show(now)));

    doremir_for_each(s, scheduler->senders) {
        doremir_message_sync(s);
    }

    for (int i = 0; i < max_events_k; ++i) {
        event_t event = doremir_priority_queue_peek(scheduler->queue);

        if (!event) {
            sched_inform(string("- No events"));
            break;
        }

        if (!doremir_less_than(doremir_event_offset(event), now)) {
            sched_inform(string("- Waiting"));
            break;
        }

        doremir_priority_queue_pop(scheduler->queue);

        sched_inform(string_dappend(string("* Due:   "), doremir_string_show(event)));
        sched_inform(string_dappend(string("    Off: "), doremir_string_show(doremir_event_offset(event))));

        if (doremir_event_has_value(now, event)) {
            ptr_t value = doremir_event_value(now, event);
            value = value; // kill warning
            sched_inform(string_dappend(string("  Value: "), doremir_string_show(value)));
        } else {
            sched_inform(string("  No value"));
        }

        if (doremir_event_has_tail(now, event)) {
            event_t tail = doremir_event_tail(now, event);
            recur[recurring++] = tail;
            sched_inform(string_dappend(string("  Tail:  "), doremir_string_show(tail)));
        } else {
            sched_inform(string("  No tail"));
        }
    }

    for (int i = 0; i < recurring; ++i) {
        doremir_priority_queue_insert(recur[i], scheduler->queue);
    }

    doremir_destroy(abs_now);
    doremir_destroy(now);
}

void doremir_scheduler_loop(doremir_scheduler_t scheduler)
{
    while (true) {
        doremir_scheduler_execute(scheduler);
        doremir_thread_sleep(loop_interval_k);
    }
}


// --------------------------------------------------------------------------------

bool scheduler_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    return a == b;
}

doremir_string_t scheduler_show(doremir_ptr_t v)
{
    string_t s = string("<Scheduler");
    s = string_dappend(s, doremir_string_format_integer(" %02x", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void scheduler_destroy(doremir_ptr_t a)
{
    doremir_scheduler_destroy(a);
}

doremir_ptr_t scheduler_impl(doremir_id_t interface)
{
    static doremir_equal_t scheduler_equal_impl = { scheduler_equal };
    static doremir_string_show_t scheduler_show_impl = { scheduler_show };
    static doremir_destroy_t scheduler_destroy_impl = { scheduler_destroy };

    switch (interface) {
    case doremir_equal_i:
        return &scheduler_equal_impl;

    case doremir_string_show_i:
        return &scheduler_show_impl;

    case doremir_destroy_i:
        return &scheduler_destroy_impl;

    default:
        return NULL;
    }
}

