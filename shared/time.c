
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/time.h>
#include <fa/util.h>

struct _fa_time_t {
    fa_impl_t          impl;       //  Interface dispatcher
    double          dvalue;      // Value in seconds
};

double to_double(fa_ratio_t x);
static ratio_t from_double(double x)
{
    return fa_ratio(x * 10000, 10000);
}

// --------------------------------------------------------------------------------

inline static fa_time_t new_time(double dvalue)
{
    fa_ptr_t time_impl(fa_id_t interface);

    fa_time_t t     = fa_new(time);
    t->impl         = &time_impl;
    t->dvalue       = dvalue;
    return t;
}

inline static void delete_time(fa_time_t time)
{
    // TODO
    // fa_ratio_destroy(time->value);
    fa_delete(time);
}

// --------------------------------------------------------------------------------

fa_time_t fa_time_create(int32_t days, int32_t hours, int32_t minutes, fa_ratio_t seconds)
{
    double whole = days * (60 * 60 * 24) + hours * (60 * 60) + minutes * 60;
    return new_time(whole + to_double(seconds));
}

fa_time_t fa_time_copy(fa_time_t time)
{
    return new_time(time->dvalue);
}

void fa_time_destroy(fa_time_t time)
{
    delete_time(time);
}


// --------------------------------------------------------------------------------

fa_ratio_t fa_time_divisions(fa_time_t time)
{
    return from_double(time->dvalue);
}

int32_t fa_time_seconds(fa_time_t time)
{
    int32_t x = time->dvalue;
    return x % 60;
}

int32_t fa_time_minutes(fa_time_t time)
{
    int32_t x = time->dvalue;
    return x % (60 * 60) / 60;
}

int32_t fa_time_hours(fa_time_t time)
{
    int32_t x = time->dvalue;
    return x % (60 * 60 * 24) / (60 * 60);
}

int32_t fa_time_days(fa_time_t time)
{
    int32_t x = time->dvalue;
    return x / (60 * 60 * 24);
}

int32_t fa_time_to_seconds(fa_time_t time)
{
    return fa_time_days(time)      * 24 * 60 * 60
           + fa_time_hours(time)   * 60 * 60
           + fa_time_minutes(time) * 60
           + fa_time_seconds(time);
}

#define approx_millis(r) ( (int32_t) ( ((double) fa_ratio_num(r)) / ((double) fa_ratio_denom(r)) * 1000 ) )

/** Convert the time to milliseconds.
    This may lose precision.

    @param time
        Time interval.
 */
fa_time_milliseconds_t fa_time_to_milliseconds(fa_time_t time)
{
    return time->dvalue * 1000.0;
}


fa_string_t fa_time_to_iso(fa_time_t time)
{
    fa_time_t t = (fa_time_t) time;
    string_t s = fa_string("P0000-00");

    s = fa_string_dappend(s, fa_format_integral("-%02i", fa_time_days(t)));
    s = fa_string_dappend(s, fa_format_integral("T%02i", fa_time_hours(t)));
    s = fa_string_dappend(s, fa_format_integral(":%02i", fa_time_minutes(t)));
    s = fa_string_dappend(s, fa_format_integral(":%02i", fa_time_seconds(t)));

    // TODO approximate ratio
    s = fa_string_dappend(s, fa_string(".0000"));

    return s;
}


// --------------------------------------------------------------------------------

bool time_equal(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return x->dvalue == y->dvalue;
}

bool time_less_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return x->dvalue < y->dvalue;
}

bool time_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return x->dvalue > y->dvalue;
}

fa_ptr_t time_add(fa_ptr_t a, fa_ptr_t b)
{
    fa_ratio_t fa_ratio_add_safe(fa_ratio_t x, fa_ratio_t y);
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(x->dvalue + y->dvalue);
}

fa_ptr_t time_subtract(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(x->dvalue - y->dvalue);
}

fa_ptr_t time_multiply(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(x->dvalue * y->dvalue);
}

fa_ptr_t time_divide(fa_ptr_t a, fa_ptr_t b)
{
    fa_time_t x = (fa_time_t) a;
    fa_time_t y = (fa_time_t) b;
    return new_time(x->dvalue / y->dvalue);
}

fa_ptr_t time_absolute(fa_ptr_t a)
{
    fa_time_t x = (fa_time_t) a;
    return new_time(fabs(x->dvalue));
}

fa_string_t time_show(fa_ptr_t a)
{
    fa_time_t t = (fa_time_t) a;
    string_t s = fa_string("<Time");

    s = fa_string_dappend(s, fa_format_integral(" %02id", fa_time_days(t)));
    s = fa_string_dappend(s, fa_format_integral(" %02ih", fa_time_hours(t)));
    s = fa_string_dappend(s, fa_format_integral(" %02im", fa_time_minutes(t)));
    s = fa_string_dappend(s, fa_format_integral(" %02i+", fa_time_seconds(t)));
    s = fa_string_dappend(s, fa_string_show(fa_time_divisions(t)));
    s = fa_string_dappend(s, fa_string("s"));
    s = fa_string_dappend(s, fa_string(">"));

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

