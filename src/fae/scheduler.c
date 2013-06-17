
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/scheduler.h>
#include <fae/priority_queue.h>
#include <fae/thread.h>
#include <fae/util.h>

/*
    Notes:
 */

struct _fae_scheduler_t {
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
    ptr_t scheduler_impl(fae_id_t interface);

    scheduler_t scheduler = fae_new(scheduler);
    scheduler->impl     = &scheduler_impl;
    scheduler->clock    = clock;
    scheduler->start    = fae_time_time(scheduler->clock);
    scheduler->last     = seconds(0);
    scheduler->senders  = fae_list_empty();
    scheduler->queue    = priority_queue();
    return scheduler;
}

inline static void delete_scheduler(scheduler_t scheduler)
{
    fae_delete(scheduler);
}


// -----------------------------------------------------------------------------

/** Schedule.
    @param clock            Must implement [Clock](@ref fae_time_clock_t).
 */
fae_scheduler_t fae_scheduler_create(fae_time_clock_t clock)
{
    return new_scheduler(clock);
}


void fae_scheduler_destroy(scheduler_t scheduler)
{
    fae_destroy(scheduler->start);
    fae_destroy(scheduler->last);
    fae_destroy(scheduler->senders);
    fae_destroy(scheduler->queue);
    delete_scheduler(scheduler);
}


static void collect_sync(fae_ptr_t senders, fae_message_sender_t sender)
{
    // Add hoc set, to user pointer equality
    list_t *list = senders;
    fae_for_each(s, *list) {
        if (s == sender) {
            return;
        }
    }
    *list = fae_list_dcons(sender, *list);
}

/** Schedule.
    @param event            Event to schedule.
    @param scheduler        The scheduler.
 */
void fae_scheduler_schedule(fae_scheduler_t scheduler, fae_event_t event)
{
    // time_t      absolute  = fae_time_time(scheduler->clock);
    // time_t      now       = fae_subtract(absolute, scheduler->start);

    fae_priority_queue_insert(delay_event(scheduler->start, event), scheduler->queue);
    fae_event_add_sync(collect_sync, &scheduler->senders, event);

    // fae_destroy(now);
    // fae_destroy(absolute);
}

#define sched_inform(str)
#define loop_interval_k 1
// #define sched_inform(str) dinform(str)
// #define loop_interval_k 100

void fae_scheduler_execute(fae_scheduler_t scheduler)
{
    time_t      absolute  = fae_time_time(scheduler->clock);
    time_t      now       = fae_subtract(absolute, scheduler->start);
    time_t      last      = fae_move(scheduler->last);
    list_t      resched   = fae_list_empty();

    sched_inform(string_dappend(string("~ "), fae_string_show(last)));
    sched_inform(string_dappend(string("@ "), fae_string_show(now)));

    fae_for_each(s, scheduler->senders) {
        fae_message_sync(s);
    }


    while (true) {
        event_t event = fae_priority_queue_peek(scheduler->queue);

        if (!event) {
            sched_inform(string("- No events"));
            break;
        }

        // if (!fae_less_than_equal(fae_event_offset(event), now)) {
        // sched_inform(string("- Waiting"));
        // break;
        // }

        sched_inform(string_dappend(string("* Due:   "), fae_string_show(event)));
        sched_inform(string_dappend(string("    Off: "), fae_string_show(fae_event_offset(event))));

        fae_priority_queue_pop(scheduler->queue);
        fae_event_values(last, now, event);

        if (fae_event_has_more(now, event)) {
            resched = fae_list_dcons(event, resched);
        }
    }

    fae_for_each(resc, resched) {
        fae_priority_queue_insert(resc, scheduler->queue);
    }

    fae_destroy(resched);
    fae_destroy(last);
    scheduler->last = fae_move(now);
    fae_destroy(absolute);
}

void fae_scheduler_loop(fae_scheduler_t scheduler)
{
    while (true) {
        fae_scheduler_execute(scheduler);
        fae_thread_sleep(loop_interval_k);
    }
}

void fae_scheduler_loop_for(time_t time, fae_scheduler_t scheduler)
{
    time_t begin = fae_time_time(scheduler->clock);
    time_t end   = fae_add(begin, time);
    
    while (true) {
        if (fae_greater_than(fae_time_time(scheduler->clock), end))
            return;
        fae_scheduler_execute(scheduler);
        fae_thread_sleep(loop_interval_k);
    }
}


// --------------------------------------------------------------------------------

bool scheduler_equal(fae_ptr_t a, fae_ptr_t b)
{
    return a == b;
}

fae_string_t scheduler_show(fae_ptr_t v)
{
    string_t s = string("<Scheduler");
    s = string_dappend(s, fae_string_format_integral(" %02x", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void scheduler_destroy(fae_ptr_t a)
{
    fae_scheduler_destroy(a);
}

fae_ptr_t scheduler_impl(fae_id_t interface)
{
    static fae_equal_t scheduler_equal_impl = { scheduler_equal };
    static fae_string_show_t scheduler_show_impl = { scheduler_show };
    static fae_destroy_t scheduler_destroy_impl = { scheduler_destroy };

    switch (interface) {
    case fae_equal_i:
        return &scheduler_equal_impl;

    case fae_string_show_i:
        return &scheduler_show_impl;

    case fae_destroy_i:
        return &scheduler_destroy_impl;

    default:
        return NULL;
    }
}

