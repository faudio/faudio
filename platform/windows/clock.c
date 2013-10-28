
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

#include <windows.h>

struct {
    LARGE_INTEGER   currTime;
    LARGE_INTEGER   offset;
    double          frequency;
} clock_data;

// --------------------------------------------------------------------------------

void fa_clock_initialize()
{
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&clock_data.offset);
    clock_data.frequency = (double) freq.QuadPart / 1000000.;
}

void fa_clock_terminate()
{
    // Nothing
}

void clock_get_time(struct timeval *tv)
{
    QueryPerformanceCounter(&clock_data.currTime);
    clock_data.currTime.QuadPart -= clock_data.offset.QuadPart;
    clock_data.currTime.QuadPart /= clock_data.frequency;
    tv->tv_sec = clock_data.currTime.QuadPart / 1000000;
    tv->tv_usec = clock_data.currTime.QuadPart % 1000000;
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
    fa_ptr_t standard_clock_impl(fa_id_t iface);
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
    struct timeval tv;
    clock_get_time(&tv);

    return (int64_t) tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

fa_time_t standard_clock_time(fa_ptr_t a)
{
    struct timeval tv;
    clock_get_time(&tv);

    time_t s  = seconds(tv.tv_sec);
    time_t ds = divisions(tv.tv_usec / 1000, 1000);
    return fa_dadd(s, ds);
}


fa_ptr_t standard_clock_impl(fa_id_t iface)
{
    static fa_string_show_t standard_clock_show_impl
        = { standard_clock_show };

    static fa_clock_interface_t standard_clock_clock_impl
        = { standard_clock_time, standard_clock_milliseconds };

    switch (iface) {

    case fa_clock_interface_i:
        return &standard_clock_clock_impl;

    case fa_string_show_i:
        return &standard_clock_show_impl;

    default:
        return NULL;
    }
}

