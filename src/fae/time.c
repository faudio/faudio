
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/time.h>
#include <fae/util.h>

#include <time.h>
#include <sys/time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

typedef fae_ratio_num_t num_t;
typedef fae_ratio_denom_t denom_t;


struct _fae_time_t {
    impl_t          impl;       //  Interface dispatcher
    ratio_t         value;      //  Value in seconds
};

static clock_serv_t mach_clock_g;

// --------------------------------------------------------------------------------

void fae_time_initialize()
{
    host_get_clock_service(mach_host_self(), REALTIME_CLOCK, &mach_clock_g);
}

void fae_time_terminate()
{
    mach_port_deallocate(mach_task_self(), mach_clock_g);
}

// --------------------------------------------------------------------------------

inline static fae_time_t new_time(ratio_t value)
{
    fae_ptr_t time_impl(fae_id_t interface);

    fae_time_t t = fae_new(time);
    t->impl  = &time_impl;
    t->value = fae_copy(value);
    // t->value = value;
    return t;           
    // TODO should not copy, but Lisp wants it (see also below)
}

inline static void delete_time(fae_time_t time)
{
    fae_ratio_destroy(time->value);
    fae_delete(time);
}

// --------------------------------------------------------------------------------

/** Create a new time interval.
    @param days
        Number of days.
    @param hours
        Number of hours.
    @param minutes
        Number of minutes.
    @param seconds
        Number of seconds (destroyed).
    @return
        A new time value.
 */
fae_time_t fae_time_create(int32_t days, int32_t hours, int32_t minutes, fae_ratio_t seconds)
{
    int  whole = days * (60 * 60 * 24) + hours * (60 * 60) + minutes * 60;
    return new_time(fae_add(ratio(whole, 1), seconds));
    // TODO should dadd, but Lisp doesn't like it
}

/**
    Copy the given time interval.
 */
fae_time_t fae_time_copy(fae_time_t time)
{
    return new_time(fae_ratio_copy(time->value));
}

/**
    Destroy the given time interval.
 */
void fae_time_destroy(fae_time_t time)
{
    delete_time(time);
}


// --------------------------------------------------------------------------------

/**
    Return the fractions of a second in this time interval.

    @param time
        Time interval.
    @return
        Rational number, representing the remainder of time in seconds over one.
 */
fae_ratio_t fae_time_divisions(fae_time_t time)
{
    num_t   a;
    denom_t b;
    fae_ratio_match(time->value, &a, &b);
    return ratio(a % b, b);
}

/**
    Return the number of whole seconds in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in seconds modulo one.
 */
int32_t fae_time_seconds(fae_time_t time)
{
    num_t   a;
    denom_t b;
    fae_ratio_match(time->value, &a, &b);
    return (a / b) % 60;
}

/**
    Return the number of whole minutes in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in minutes modulo one.
 */
int32_t fae_time_minutes(fae_time_t time)
{
    num_t   a;
    denom_t b;
    fae_ratio_match(time->value, &a, &b);
    return (a / b) % (60 * 60) / 60;
}

/**
    Return the number of whole hours in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in hours modulo one.
 */
int32_t fae_time_hours(fae_time_t time)
{
    num_t   a;
    denom_t b;
    fae_ratio_match(time->value, &a, &b);
    return (a / b) % (60 * 60 * 24) / (60 * 60);
}

/**
    Return the number of whole days in this time interval.

    @param time
        Time interval.
    @return
        Integer representing this time in days modulo one.
 */
int32_t fae_time_days(fae_time_t time)
{
    num_t   a;
    denom_t b;
    fae_ratio_match(time->value, &a, &b);
    return (a / b) / (60 * 60 * 24);
}

/** Convert the time to seconds.
    This may lose precision.

    @param time
        Time interval.
 */
int32_t fae_time_to_seconds(fae_time_t time)
{
    return fae_time_days(time)    * 24 * 60 * 60
           + fae_time_hours(time)   * 60 * 60
           + fae_time_minutes(time) * 60
           + fae_time_seconds(time);
}

/** Convert the time to milliseconds.
    This may lose precision.

    @param time
        Time interval.
 */
int32_t fae_time_to_milliseconds(fae_time_t time)
{
    assert(false && "Not implemented");
}


/** Print the time as an ISO 8601 duration.

    The ISO represenation use decimal fractions of a second, and may lose precision. For example
    the duration of 1 min 24 1/3 sec would be represented as `P0000-00-00T00:01:24.3333`.
 */
fae_string_t fae_time_to_iso(fae_time_t time)
{
    fae_time_t t = (fae_time_t) time;
    string_t s = string("P0000-00");

    s = string_dappend(s, format_integral("-%02i", fae_time_days(t)));
    s = string_dappend(s, format_integral("T%02i", fae_time_hours(t)));
    s = string_dappend(s, format_integral(":%02i", fae_time_minutes(t)));
    s = string_dappend(s, format_integral(":%02i", fae_time_seconds(t)));
    // TODO approximate ratio
    s = string_dappend(s, string(".0000"));

    return s;
}

/** Convert system time to a time interval.
    Generally, system time is seconds since the Unix epoch.
 */
fae_time_t fae_time_from_system(fae_time_system_t time)
{
    // return seconds(ti64(time));
    assert(false && "Not implemented");
}

/** Convert system CPU time to a time interval.
 */
fae_time_t fae_time_from_cpu(fae_time_cpu_t cpu_time)
{
    // int64_t t = fae_peek_int64(cpu_time);
    // int64_t q = t / CLOCKS_PER_SEC;
    // int64_t r = t % CLOCKS_PER_SEC;
    //
    // return fae_add(seconds(q), divisions(r, CLOCKS_PER_SEC));
    assert(false && "Not implemented");
}

/** Get the system time.
 */
fae_time_system_t fae_time_system()
{
    // // TODO warning OK
    // system_time_t t;
    // time(&t);
    // int64_t lt = t;
    // return i64(lt);
    assert(false && "Not implemented");
}

/** Get the system CPU time.
 */
fae_time_cpu_t fae_time_cpu()
{
    // // TODO warning OK
    // system_clock_t t = clock();
    // int64_t lt = t;
    // return i64(lt);
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

fae_time_t fae_time_time(fae_time_clock_t clock)
{
    assert(fae_interface(fae_time_clock_interface_i, clock)
           && "Must implement Clock");
    return ((fae_time_clock_interface_t *)
            fae_interface(fae_time_clock_interface_i, clock))->time(clock);
}

double fae_time_tick_rate(fae_time_clock_t clock)
{
    assert(fae_interface(fae_time_clock_interface_i, clock)
           && "Must implement Clock");
    return ((fae_time_clock_interface_t *)
            fae_interface(fae_time_clock_interface_i, clock))->tick_rate(clock);
}

int64_t fae_time_ticks(fae_time_clock_t clock)
{
    assert(fae_interface(fae_time_clock_interface_i, clock)
           && "Must implement Clock");
    return ((fae_time_clock_interface_t *)
            fae_interface(fae_time_clock_interface_i, clock))->ticks(clock);
}


// --------------------------------------------------------------------------------

struct system_clock {
    impl_t impl;
};
typedef struct system_clock *system_clock_t;

ptr_t system_clock_impl(fae_id_t interface);

clock_t fae_time_get_system_clock()
{
    system_clock_t clock = fae_new_struct(system_clock);
    clock->impl = &system_clock_impl;
    return (clock_t) clock;
}

double system_tick_rate(ptr_t a)
{
    return 1;
}

int64_t system_ticks(ptr_t a)
{
    // system_time_t t;
    // time(&t);
    // int64_t lt = t;
    // return lt;
    assert(false && "Not implemented");
}

fae_time_t system_time(ptr_t a)
{
    return fae_time_from_system(fae_time_system());
}

ptr_t system_clock_impl(fae_id_t interface)
{
    static fae_time_clock_interface_t system_clock_clock
        = { system_time, system_tick_rate, system_ticks };

    switch (interface) {

    case fae_time_clock_interface_i:
        return &system_clock_clock;

    default:
        return NULL;
    }
}



// --------------------------------------------------------------------------------

struct system_prec_clock {
    impl_t impl;
};
typedef struct system_prec_clock *system_prec_clock_t;

ptr_t system_prec_clock_impl(fae_id_t interface);

clock_t fae_time_get_system_prec_clock()
{
    system_prec_clock_t clock = fae_new_struct(system_prec_clock);
    clock->impl = &system_prec_clock_impl;
    return (clock_t) clock;
}

double system_prec_tick_rate(ptr_t a)
{
    return 1000000000;
}

// TODO separate init/term
int64_t system_prec_ticks(ptr_t a)
{
    mach_timespec_t ts;
    clock_get_time(mach_clock_g, &ts);

    return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

fae_time_t system_prec_time(ptr_t a)
{
    mach_timespec_t ts;
    clock_get_time(mach_clock_g, &ts);

    // clock_gettime(CLOCK_REALTIME, &ts);

    time_t s  = seconds(ts.tv_sec); // TODO with tv_nsec
    time_t ds = divisions(ts.tv_nsec / 1000000, 1000);
    return fae_dadd(s, ds);


}

ptr_t system_prec_clock_impl(fae_id_t interface)
{
    static fae_time_clock_interface_t system_prec_clock_clock
        = { system_prec_time, system_prec_tick_rate, system_prec_ticks };

    switch (interface) {

    case fae_time_clock_interface_i:
        return &system_prec_clock_clock;

    default:
        return NULL;
    }
}



// --------------------------------------------------------------------------------

bool time_equal(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return fae_equal(x->value, y->value);
}

bool time_less_than(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return fae_less_than(x->value, y->value);
}

bool time_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return fae_greater_than(x->value, y->value);
}

fae_ptr_t time_add(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return new_time(fae_add(x->value, y->value));
}

fae_ptr_t time_subtract(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return new_time(fae_subtract(x->value, y->value));
}

fae_ptr_t time_multiply(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return new_time(fae_multiply(x->value, y->value));
}

fae_ptr_t time_divide(fae_ptr_t a, fae_ptr_t b)
{
    fae_time_t x = (fae_time_t) a;
    fae_time_t y = (fae_time_t) b;
    return new_time(fae_divide(x->value, y->value));
}

fae_ptr_t time_absolute(fae_ptr_t a)
{
    fae_time_t x = (fae_time_t) a;
    return new_time(fae_absolute(x->value));
}

fae_string_t time_show(fae_ptr_t a)
{
    fae_time_t t = (fae_time_t) a;
    string_t s = string("<Time");

    s = string_dappend(s, format_integral(" %02id", fae_time_days(t)));
    s = string_dappend(s, format_integral(" %02ih", fae_time_hours(t)));
    s = string_dappend(s, format_integral(" %02im", fae_time_minutes(t)));
    s = string_dappend(s, format_integral(" %02i+", fae_time_seconds(t)));
    s = string_dappend(s, fae_string_show(fae_time_divisions(t)));
    s = string_dappend(s, string("s"));
    s = string_dappend(s, string(">"));
// fae_print("ratio: %s\n", t->value);
// fae_print("ratio: %s\n", s);

    return s;
}

fae_ptr_t time_copy(fae_ptr_t a)
{
    return fae_time_copy(a);
}

void time_destroy(fae_ptr_t a)
{
    fae_time_destroy(a);
}

fae_ptr_t time_impl(fae_id_t interface)
{
    static fae_equal_t time_equal_impl
        = { time_equal };
    static fae_order_t time_order_impl
        = { time_less_than, time_greater_than };
    static fae_string_show_t time_show_impl
        = { time_show };
    static fae_number_t  time_number_impl
        = { time_add, time_subtract, time_multiply, time_divide, time_absolute };
    static fae_copy_t time_copy_impl
        = { time_copy };
    static fae_destroy_t time_destroy_impl
        = { time_destroy };


    switch (interface) {
    case fae_equal_i:
        return &time_equal_impl;

    case fae_order_i:
        return &time_order_impl;

    case fae_string_show_i:
        return &time_show_impl;

    case fae_number_i:
        return &time_number_impl;

    case fae_copy_i:
        return &time_copy_impl;

    case fae_destroy_i:
        return &time_destroy_impl;

    default:
        return NULL;
    }
}

