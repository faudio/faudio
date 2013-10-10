
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/time.h>
#include <fa/clock.h>
// #include <fa/util.h>

#include <time.h>
#include <sys/time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

static clock_serv_t gMachClock;

// --------------------------------------------------------------------------------

void fa_clock_initialize()
{
    host_get_clock_service(mach_host_self(), REALTIME_CLOCK, &gMachClock);
}

void fa_clock_terminate()
{
    mach_port_deallocate(mach_task_self(), gMachClock);
}

// --------------------------------------------------------------------------------

fa_time_t 	fa_clock_time (fa_clock_t clock)
{
    assert(fa_interface(fa_clock_interface_i, clock) && "Must implement Clock");
    return ((fa_clock_interface_t *) fa_interface(fa_clock_interface_i, clock))->time(clock);
    
}
 
fa_time_milliseconds_t 	fa_clock_milliseconds (fa_clock_t clock)
{
    assert(fa_interface(fa_clock_interface_i, clock) && "Must implement Clock");
    return ((fa_clock_interface_t *) fa_interface(fa_clock_interface_i, clock))->milliseconds(clock);
}

// --------------------------------------------------------------------------------

// struct system_clock {
//     impl_t impl;
// };
// typedef struct system_clock *system_clock_t;
//
// ptr_t system_clock_impl(fa_id_t interface);
//
// clock_t fa_time_get_system_clock()
// {
//     system_clock_t clock = fa_new_struct(system_clock);
//     clock->impl = &system_clock_impl;
//     return (clock_t) clock;
// }
//
// double system_tick_rate(ptr_t a)
// {
//     return 1;
// }
//
// int64_t system_ticks(ptr_t a)
// {
//     // system_time_t t;
//     // time(&t);
//     // int64_t lt = t;
//     // return lt;
//     assert(false && "Not implemented");
// }
//
// fa_time_t system_time(ptr_t a)
// {
//     return fa_time_from_system(fa_time_system());
// }
//
// ptr_t system_clock_impl(fa_id_t interface)
// {
//     static fa_clock_interface_t system_clock_clock
//         = { system_time, system_tick_rate, system_ticks };
//
//     switch (interface) {
//
//     case fa_clock_interface_i:
//         return &system_clock_clock;
//
//     default:
//         return NULL;
//     }
// }
//
//
//
// // --------------------------------------------------------------------------------
//
// struct system_prec_clock {
//     impl_t impl;
// };
// typedef struct system_prec_clock *system_prec_clock_t;
//
// ptr_t system_prec_clock_impl(fa_id_t interface);
//
// clock_t fa_time_get_system_prec_clock()
// {
//     system_prec_clock_t clock = fa_new_struct(system_prec_clock);
//     clock->impl = &system_prec_clock_impl;
//     return (clock_t) clock;
// }
//
// double system_prec_tick_rate(ptr_t a)
// {
//     return 1000000000;
// }
//
// // TODO separate init/term
// int64_t system_prec_ticks(ptr_t a)
// {
//     mach_timespec_t ts;
//     clock_get_time(gMachClock, &ts);
//
//     return ts.tv_sec * 1000000000 + ts.tv_nsec;
// }
//
// fa_time_t system_prec_time(ptr_t a)
// {
//     mach_timespec_t ts;
//     clock_get_time(gMachClock, &ts);
//
//     // clock_gettime(CLOCK_REALTIME, &ts);
//
//     time_t s  = seconds(ts.tv_sec); // TODO with tv_nsec
//     time_t ds = divisions(ts.tv_nsec / 1000000, 1000);
//     return fa_dadd(s, ds);
//
//
// }
//
// ptr_t system_prec_clock_impl(fa_id_t interface)
// {
//     static fa_clock_interface_t system_prec_clock_clock
//         = { system_prec_time, system_prec_tick_rate, system_prec_ticks };
//
//     switch (interface) {
//
//     case fa_clock_interface_i:
//         return &system_prec_clock_clock;
//
//     default:
//         return NULL;
//     }
// }