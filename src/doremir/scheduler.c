
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/scheduler.h>
#include <doremir/priority_queue.h>
#include <doremir/util.h>

/*  Notes:
 */

typedef doremir_scheduler_action_t  action_t;
typedef doremir_scheduler_t         scheduler_t;        
typedef doremir_thread_improving_t  improving_t;


struct _doremir_scheduler_t {
    impl_t                  impl;           // Dispatcher

    improving_t             time;           // Current time
    priority_queue_t        queue;          // Enqueued values
    
};

ptr_t scheduler_impl(doremir_id_t interface);

inline static scheduler_t new_scheduler() 
{
    scheduler_t scheduler = doremir_new(scheduler);
    scheduler->impl = &scheduler_impl;
    return scheduler;
}

inline static void delete_scheduler(scheduler_t scheduler) 
{
    doremir_delete(scheduler);
}


// -----------------------------------------------------------------------------

doremir_scheduler_t doremir_scheduler_create(improving_t improving)
{
    return new_scheduler(); // TODO
}

void doremir_scheduler_destroy(scheduler_t scheduler)
{
    // destroy improving
    // destroy queue
    delete_scheduler(scheduler);
}

void doremir_scheduler_schedule(scheduler_t scheduler,
                                time_t      time,
                                action_t    action,
                                ptr_t       data)
{
    // make entry
    // insert into queue
}

void doremir_scheduler_execute(scheduler_t scheduler)
{
    // now = get_time()
    // while (peek(q)->time < now)
        // if (!action interrupded or canceled)
            // execute
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
    
    switch (interface)
    {
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



