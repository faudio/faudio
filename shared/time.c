
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/time.h>
#include <fa/util.h>

#include <time.h>
#include <sys/time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

typedef fa_ratio_num_t num_t;
typedef fa_ratio_denom_t denom_t;


struct _fa_time_t {
    impl_t          impl;       //  Interface dispatcher
    ratio_t         value;      //  Value in seconds
};

static clock_serv_t mach_clock_g;

// --------------------------------------------------------------------------------

void fa_time_initialize()
{
    host_get_clock_service(mach_host_self(), REALTIME_CLOCK, &mach_clock_g);
}

void fa_time_terminate()
{
    mach_port_deallocate(mach_task_self(), mach_clock_g);
}

// --------------------------------------------------------------------------------

inline static fa_time_t new_time(ratio_t value)
{
    fa_ptr_t time_impl(fa_id_t interface);

    fa_time_t t = fa_new(time);
    t->impl  = &time_impl;
    t->value = fa_copy(value);
    // t->value = value;
    return t;
    // TODO should not copy, but Lisp wants it (see also below)
}

inline static void delete_time(fa_time_t time)
{
    fa_ratio_destroy(time->value);
    fa_delete(time);
}

// --------------------------------------------------------------------------------

fa_time_t fa_time_create(int32_t days, int32_t hours, int32_t minutes, fa_ratio_t seconds)
{
    int  whole = days * (60 * 60 * 24) + hours * (60 * 60) + minutes * 60;
    return new_time(fa_add(ratio(whole, 1), seconds));
    // TODO should dadd, but Lisp doesn't like it
}

fa_time_t fa_time_copy(fa_time_t time)
{
    return new_time(fa_ratio_copy(time->value));
}

void fa_time_destroy(fa_time_t time)
{
    delete_time(time);
}


// --------------------------------------------------------------------------------

fa_ratio_t fa_time_divisions(fa_time_t time)
{
    num_t   a;
    denom_t b;
    fa_ratio_match(time->value, &a, &b);
    return ratio(a % b, b);
}

int32_t fa_time_seconds(fa_time_t time)
{
    num_t   a;
    denom_t b;
    fa_ratio_match(time->value, &a, &b);
    return (a / b) % 60;
}

int32_t fa_time_minutes(fa_time_t time)
{
    num_t   a;
    denom_t b;
    fa_ratio_match(time->value, &a, &b);
    return (a / b) % (60 * 60) / 60;
}

int32_t fa_time_hours(fa_time_t time)
{
    num_t   a;
    denom_t b;
    fa_ratio_match(time->value, &a, &b);
    return (a / b) % (60 * 60 * 24) / (60 * 60);
}

int32_t fa_time_days(fa_time_t time)
{
    num_t   a;
    denom_t b;
    fa_ratio_match(time->value, &a, &b);
    return (a / b) / (60 * 60 * 24);
}

int32_t fa_time_to_seconds(fa_time_t time)
{
    return fa_time_days(time)    * 24 * 60 * 60
           + fa_time_hours(time)   * 60 * 60
           + fa_time_minutes(time) * 60
           + fa_time_seconds(time);
}

#define approx_ratio(r) ( ((double) fa_ratio_num(r)) / ((double) fa_ratio_denom(r)) )
/** Convert the time to milliseconds.
    This may lose precision.

    @param time
        Time interval.
 */
int32_t fa_time_to_milliseconds(fa_time_t time)
{            
    // TODO very slow
    return fa_time_days(time)      * 24 * 60 * 60 * 1000
           + fa_time_hours(time)   * 60 * 60 * 1000
           + fa_time_minutes(time) * 60 * 1000
           + fa_time_seconds(time) * 1000
           + approx_ratio(fa_time_divisions(time));
}


fa_string_t fa_time_to_iso(fa_time_t time)
{
    fa_time_t t = (fa_time_t) time;
    string_t s = string("P0000-00");

    s = string_dappend(s, format_integral("-%02i", fa_time_days(t)));
    s = string_dappend(s, format_integral("T%02i", fa_time_hours(t)));
    s = string_dappend(s, format_integral(":%02i", fa_time_minutes(t)));
    s = string_dappend(s, format_integral(":%02i", fa_time_seconds(t)));

    // TODO approximate ratio
    s = string_dappend(s, string(".0000"));

    return s;
}

fa_time_t fa_time_from_system(fa_time_system_t time)
{
    // return seconds(ti64(time));
    assert(false && "Not implemented");
}

fa_time_t fa_time_from_cpu(fa_time_cpu_t cpu_time)
{
    // int64_t t = fa_peek_int64(cpu_time);
    // int64_t q = t / CLOCKS_PER_SEC;
    // int64_t r = t % CLOCKS_PER_SEC;
    //
    // return fa_add(seconds(q), divisions(r, CLOCKS_PER_SEC));
    assert(false && "Not implemented");
}

fa_time_system_t fa_time_system()
{
    // // TODO warning OK
    // system_time_t t;
    // time(&t);
    // int64_t lt = t;
    // return i64(lt);
    assert(false && "Not implemented");
}

fa_time_cpu_t fa_time_cpu()
{
    // // TODO warning OK
    // system_clock_t t = clock();
    // int64_t lt = t;
    // return i64(lt);
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

// fa_time_t fa_time_time(fa_time_clock_t clock)
// {
//     assert(fa_interface(fa_time_clock_interface_i, clock)
//            && "Must implement Clock");
//     return ((fa_time_clock_interface_t *)
//             fa_interface(fa_time_clock_interface_i, clock))->time(clock);
// }
//
// double fa_time_tick_rate(fa_time_clock_t clock)
// {
//     assert(fa_interface(fa_time_clock_interface_i, clock)
//            && "Must implement Clock");
//     return ((fa_time_clock_interface_t *)
//             fa_interface(fa_time_clock_interface_i, clock))->tick_rate(clock);
// }
//
// int64_t fa_time_ticks(fa_time_clock_t clock)
// {
//     assert(fa_interface(fa_time_clock_interface_i, clock)
//            && "Must implement Clock");
//     return ((fa_time_clock_interface_t *)
//             fa_interface(fa_time_clock_interface_i, clock))->ticks(clock);
// }


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
//     static fa_time_clock_interface_t system_clock_clock
//         = { system_time, system_tick_rate, system_ticks };
//
//     switch (interface) {
//
//     case fa_time_clock_interface_i:
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
//     clock_get_time(mach_clock_g, &ts);
//
//     return ts.tv_sec * 1000000000 + ts.tv_nsec;
// }
//
// fa_time_t system_prec_time(ptr_t a)
// {
//     mach_timespec_t ts;
//     clock_get_time(mach_clock_g, &ts);
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
//     static fa_time_clock_interface_t system_prec_clock_clock
//         = { system_prec_time, system_prec_tick_rate, system_prec_ticks };
//
//     switch (interface) {
//
//     case fa_time_clock_interface_i:
//         return &system_prec_clock_clock;
//
//     default:
//         return NULL;
//     }
// }



// --------------------------------------------------------------------------------

bool time_equal(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return fa_equal(x->value, y->value);
}

bool time_less_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return fa_less_than(x->value, y->value);
}

bool time_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return fa_greater_than(x->value, y->value);
}

fa_ptr_t time_add(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(fa_add(x->value, y->value));
}

fa_ptr_t time_subtract(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(fa_subtract(x->value, y->value));
}

fa_ptr_t time_multiply(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(fa_multiply(x->value, y->value));
}

fa_ptr_t time_divide(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(fa_divide(x->value, y->value));
}

fa_ptr_t time_absolute(fa_ptr_t a)
{
    fa_time_t x = (fa_time_t) a;
    return new_time(fa_absolute(x->value));
}

fa_string_t time_show(fa_ptr_t a)
{
    fa_time_t t = (fa_time_t) a;
    string_t s = string("<Time");

    s = string_dappend(s, format_integral(" %02id", fa_time_days(t)));
    s = string_dappend(s, format_integral(" %02ih", fa_time_hours(t)));
    s = string_dappend(s, format_integral(" %02im", fa_time_minutes(t)));
    s = string_dappend(s, format_integral(" %02i+", fa_time_seconds(t)));
    s = string_dappend(s, fa_string_show(fa_time_divisions(t)));
    s = string_dappend(s, string("s"));
    s = string_dappend(s, string(">"));
// fa_print("ratio: %s\n", t->value);
// fa_print("ratio: %s\n", s);

    return s;
}

fa_ptr_t time_copy(fa_ptr_t a)
{
    return fa_time_copy(a);
}

void time_destroy(fa_ptr_t a)
{
    fa_time_destroy(a);
}

fa_ptr_t time_impl(fa_id_t interface)
{
    static fa_equal_t time_equal_impl
        = { time_equal };
    static fa_order_t time_order_impl
        = { time_less_than, time_greater_than };
    static fa_string_show_t time_show_impl
        = { time_show };
    static fa_number_t  time_number_impl
        = { time_add, time_subtract, time_multiply, time_divide, time_absolute };
    static fa_copy_t time_copy_impl
        = { time_copy };
    static fa_destroy_t time_destroy_impl
        = { time_destroy };


    switch (interface) {
    case fa_equal_i:
        return &time_equal_impl;

    case fa_order_i:
        return &time_order_impl;

    case fa_string_show_i:
        return &time_show_impl;

    case fa_number_i:
        return &time_number_impl;

    case fa_copy_i:
        return &time_copy_impl;

    case fa_destroy_i:
        return &time_destroy_impl;

    default:
        return NULL;
    }
}

