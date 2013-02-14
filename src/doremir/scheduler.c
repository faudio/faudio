
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

struct _doremir_scheduler_t {
    impl_t                  impl;           // Dispatcher
    clock_t                 clock;
    time_t                  start;
    priority_queue_t        queue;
};


// -----------------------------------------------------------------------------

inline static scheduler_t new_scheduler(clock_t clock)
{
    ptr_t scheduler_impl(doremir_id_t interface);

    scheduler_t scheduler = doremir_new(scheduler);
    scheduler->impl     = &scheduler_impl;
    scheduler->clock    = clock;
    scheduler->queue    = priority_queue();
    scheduler->start    = doremir_time_time(scheduler->clock);
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

/** Schedule.
    @param event            Event to schedule.
    @param scheduler        The scheduler.
 */
void doremir_scheduler_schedule(doremir_scheduler_t scheduler, doremir_event_t event)
{
    // warn(string_dappend(string("Inserting "), doremir_string_show(event)));
    doremir_priority_queue_insert(event, scheduler->queue);
}

#define sched_inform(str)
// #define sched_inform(str) dinform(str)

#define max_events_k 5000

void doremir_scheduler_execute(doremir_scheduler_t scheduler)
{
    time_t abs_now = doremir_time_time(scheduler->clock);
    time_t now = doremir_subtract(abs_now, scheduler->start);

    event_t reschedule[max_events_k];
    size_t  num_reschedule = 0;

    for (int i = 0; i < max_events_k; ++i) {
        sched_inform(string_dappend(string("@ "), doremir_string_show(now)));

        event_t event = doremir_priority_queue_peek(scheduler->queue);

        if (event) {
            doremir_event_sync(event);
        } else {
            sched_inform(string("- No events"));
            break;
        }

        if (!doremir_less_than(doremir_event_offset(event), now)) {
            sched_inform(string("- Waiting"));
            break;

        } else {
            sched_inform(string_dappend(string("X Due "), doremir_string_show(event)));
            doremir_priority_queue_pop(scheduler->queue);

            if (doremir_event_has_value(now, event)) {
                ptr_t   value = doremir_event_value(event);
                sched_inform(string_dappend(string("Value is: "), doremir_string_show(value)));
            }

            event_t tail  = doremir_event_tail(event);

            if (!doremir_event_is_never(tail)) {
                event_t event2 = tail;
                sched_inform(string_dappend(string("Reinsert: "), doremir_string_show(event2)));
                reschedule[num_reschedule++] = event2;
            }
        }
    }

    for (int i = 0; i < num_reschedule; ++i)
    {
        doremir_priority_queue_insert(reschedule[i], scheduler->queue);
    }

    doremir_destroy(abs_now);
    doremir_destroy(now);
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

