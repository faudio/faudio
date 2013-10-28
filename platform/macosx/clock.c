
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/time.h>
#include <fa/clock.h>
#include <fa/util.h>

#include <time.h>
#include <sys/time.h>

#include <mach/clock.h>
#include <mach/mach.h>

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

fa_time_t fa_clock_time(fa_clock_t clock)
{
    assert(fa_interface(fa_clock_interface_i, clock) && "Must implement Clock");
    return ((fa_clock_interface_t *) fa_interface(fa_clock_interface_i, clock))->time(clock);

}

fa_time_milliseconds_t fa_clock_milliseconds(fa_clock_t clock)
{
    assert(fa_interface(fa_clock_interface_i, clock) && "Must implement Clock");
    return ((fa_clock_interface_t *) fa_interface(fa_clock_interface_i, clock))->milliseconds(clock);
}


// --------------------------------------------------------------------------------

typedef struct standard_clock *standard_clock_t;
struct standard_clock {
    impl_t impl;
};

inline static standard_clock_t new_standard_clock()
{
    fa_ptr_t standard_clock_impl(fa_id_t interface);
    standard_clock_t c = fa_new_struct(standard_clock);
    c->impl = &standard_clock_impl;
    return c;
}

inline static void delete_standard_clock(standard_clock_t standard_clock)
{
    fa_delete(standard_clock);
}

fa_clock_t fa_clock_standard()
{
    return (fa_clock_t) new_standard_clock(); // TODO singleton
}


fa_string_t standard_clock_show(fa_ptr_t a)
{
    string_t str = string("<StandardClock ");
    str = string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

int64_t standard_clock_milliseconds(fa_ptr_t a)
{
    mach_timespec_t ts;
    clock_get_time(gMachClock, &ts);

    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

fa_time_t standard_clock_time(fa_ptr_t a)
{
    mach_timespec_t ts;
    clock_get_time(gMachClock, &ts);
    // clock_gettime(CLOCK_REALTIME, &ts);

    time_t s  = seconds(ts.tv_sec); // TODO with tv_nsec
    time_t ds = divisions(ts.tv_nsec / 1000000, 1000);
    return fa_dadd(s, ds);
}


fa_ptr_t standard_clock_impl(fa_id_t interface)
{
    static fa_string_show_t standard_clock_show_impl
        = { standard_clock_show };

    static fa_clock_interface_t standard_clock_clock_impl
        = { standard_clock_time, standard_clock_milliseconds };

    switch (interface) {

    case fa_clock_interface_i:
        return &standard_clock_clock_impl;

    case fa_string_show_i:
        return &standard_clock_show_impl;

    default:
        return NULL;
    }
}

