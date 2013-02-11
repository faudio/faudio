
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

typedef doremir_scheduler_t scheduler_t;


struct _doremir_scheduler_t {
    impl_t                  impl;           // Dispatcher
    ptr_t                   clock;
    priority_queue_t        queue;
};


// -----------------------------------------------------------------------------

inline static scheduler_t new_scheduler(ptr_t clock)
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
doremir_scheduler_t doremir_scheduler_create(doremir_ptr_t clock)
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
    // TODO check if already due?
    doremir_priority_queue_insert(event, scheduler->queue);
}

void doremir_scheduler_execute(doremir_scheduler_t scheduler)
{
    time_t now = doremir_time_time(scheduler->clock);
    event_t event;

    while ((event = doremir_priority_queue_peek(scheduler->queue))) {

        doremir_audio_engine_log_info(
            string_dappend(string("Peek at "), 
                           doremir_string_show(now)));

        if (!doremir_event_has_value(now, event)) {
            // return doremir_event_offset(event);

        } else {
            doremir_priority_queue_pop(scheduler->queue);
            
            ptr_t   head_event = doremir_event_value(event);
            event_t tail_event = doremir_event_tail(event);

            // doremir_audio_engine_log_error(string_dappend(string("Firing event: "), doremir_string_show(h)));

            // TODO should compare against never?
            if (tail_event) {
                // doremir_audio_engine_log_warning(string("Reinsert"));
                doremir_priority_queue_insert(tail_event, scheduler->queue);
            }
        }
    }

    // what to return?
    // return minutes(-1);
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

