
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
    delete_scheduler(scheduler);
}

/** Schedule.
    @param event            Event to schedule.
    @param scheduler        The scheduler.
 */
void doremir_scheduler_schedule(doremir_scheduler_t scheduler, doremir_event_t event)
{
    // TODO locking?
    warn(string_dappend(string("Inserting "), doremir_string_show(event)));

    time_t now = doremir_time_time(scheduler->clock);
    doremir_priority_queue_insert(delay_event(now, event), scheduler->queue);
}

void doremir_scheduler_execute(doremir_scheduler_t scheduler)
{
    time_t now = doremir_time_time(scheduler->clock);

    while (true) {
        printf("\n");
        doremir_audio_engine_log_info(string_dappend(string("Peek at "), doremir_string_show(now)));

        event_t event = doremir_priority_queue_peek(scheduler->queue);

        if (!event) {
            fail(string("No events"));
            break;

        } else if (!doremir_less_than(doremir_event_offset(event), now)) {

            fail(string("No due events"));
            warn(string_dappend(string("Offset is "), doremir_string_show(doremir_event_offset(event))));
            break;

        } else {
            warn(string_dappend(string("Due event: "), doremir_string_show(event)));

            // We now the event is due, extract it
            doremir_priority_queue_pop(scheduler->queue);

            if (!doremir_event_has_value(now, event)) {
                // Did not happen

            } else {
                ptr_t   value = doremir_event_value(event);
                event_t tail  = doremir_event_tail(event);

                value = value; // Not used, called for side effect

                if (tail) {
                    // doremir_audio_engine_log_warning(string("Reinsert"));
                    doremir_priority_queue_insert(tail, scheduler->queue);
                }
            }
        }
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

