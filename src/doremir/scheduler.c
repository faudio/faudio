
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/scheduler.h>
#include <doremir/priority_queue.h>
#include <doremir/thread.h>
#include <doremir/util.h>

/*
    Notes:
 */

struct _doremir_scheduler_t {
    impl_t                  impl;           //  Dispatcher
    clock_t                 clock;          //  Provides time
    time_t                  start;          //  Absolute time of create()
    time_t                  last;           //  Relative time of last exec()
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
    scheduler->last     = seconds(0);
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
    doremir_destroy(scheduler->start);
    doremir_destroy(scheduler->last);
    doremir_destroy(scheduler->senders);
    doremir_destroy(scheduler->queue);
    delete_scheduler(scheduler);
}


static void collect_sync(doremir_ptr_t senders, doremir_message_sender_t sender)
{
    // Add hoc set, to user pointer equality
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
#define loop_interval_k 1
// #define sched_inform(str) dinform(str)
// #define loop_interval_k 100

void doremir_scheduler_execute(doremir_scheduler_t scheduler)
{
    time_t      absolute  = doremir_time_time(scheduler->clock);
    time_t      now       = doremir_subtract(absolute, scheduler->start);
    time_t      last      = doremir_move(scheduler->last);
    list_t      resched   = doremir_list_empty();

    sched_inform(string_dappend(string("~ "), doremir_string_show(last)));
    sched_inform(string_dappend(string("@ "), doremir_string_show(now)));

    doremir_for_each(s, scheduler->senders) {
        doremir_message_sync(s);
    }


    while (true) {
        event_t event = doremir_priority_queue_peek(scheduler->queue);

        if (!event) {
            sched_inform(string("- No events"));
            break;
        }

        // if (!doremir_less_than_equal(doremir_event_offset(event), now)) {
        // sched_inform(string("- Waiting"));
        // break;
        // }

        sched_inform(string_dappend(string("* Due:   "), doremir_string_show(event)));
        sched_inform(string_dappend(string("    Off: "), doremir_string_show(doremir_event_offset(event))));

        doremir_priority_queue_pop(scheduler->queue);
        doremir_event_values(last, now, event);

        if (doremir_event_has_more(now, event)) {
            resched = doremir_list_dcons(event, resched);
        }
    }

    doremir_for_each(resc, resched) {
        doremir_priority_queue_insert(resc, scheduler->queue);
    }

    doremir_destroy(resched);
    doremir_destroy(last);
    scheduler->last = doremir_move(now);
    doremir_destroy(absolute);
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

