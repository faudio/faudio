
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/time.h>
#include <doremir/util.h>

doremir_ptr_t time_impl(doremir_id_t interface);

struct _doremir_time_t {
        impl_t          impl;       //  Interface dispatcher
        ratio_t         value;      //  Value in seconds
    };

inline static doremir_time_t
new_time(ratio_t value)
{
    doremir_time_t t = doremir_new(time);
    t->impl  = &time_impl;
    t->value = doremir_copy(value);
    return t;
}

void delete_time(doremir_time_t time)
{
    doremir_ratio_destroy(time->value);
    doremir_delete(time);
}

// --------------------------------------------------------------------------------

/** Create a new time interval.
    @param days     Number of days.
    @param hours    Number of hours.
    @param minutes  Number of minutes.
    @param seconds  Number of seconds.
    @return         A new time value.
 */
doremir_time_t doremir_time_create(int32_t days, 
                                   int32_t hours, 
                                   int32_t minutes, 
                                   doremir_ratio_t seconds)
{
    ratio_t value = doremir_add(
        ratio(days    * 60 * 60 * 24 
            + hours   * 60 * 60 
            + minutes * 60, 
            1), 
        seconds); 
    
    // TODO leaks ratio
    return new_time(value);
}

/** Copy the given time interval.
 */
doremir_time_t doremir_time_copy(doremir_time_t time)
{
    return new_time(time->value);
}

/** Destroy the given time interval.
 */
void doremir_time_destroy(doremir_time_t time)
{
    delete_time(time);
}


// --------------------------------------------------------------------------------

/**
    Return the fractions of a second in this time interval.
 */
doremir_ratio_t doremir_time_divisions(doremir_time_t time)
{
    int32_t a, b;
    doremir_ratio_match(time->value, &a, &b);
    return ratio(a % b, b);
}


/**
    Return the number of whole seconds in this time interval.
 */
int32_t doremir_time_seconds(doremir_time_t time)
{
    int32_t a, b;
    doremir_ratio_match(time->value, &a, &b);
    return (a/b) % 60;
}


/**
    Return the number of whole minutes in this time interval.
 */
int32_t doremir_time_minutes(doremir_time_t time)
{
    int32_t a, b;
    doremir_ratio_match(time->value, &a, &b);
    return (a/b) % (60*60) / 60;
}

/**
    Return the number of whole hours in this time interval.
 */
int32_t doremir_time_hours(doremir_time_t time)
{
    int32_t a, b;
    doremir_ratio_match(time->value, &a, &b);
    return (a/b) % (60*60*24) / (60*60);
}


/**
    Return the number of whole days in this time interval.
 */
int32_t doremir_time_days(doremir_time_t time)
{
    int32_t a, b;
    doremir_ratio_match(time->value, &a, &b);
    return (a/b) / (60*60*24);
}



/**
    Print the time as an ISO 8601 duration.

    The ISO represenation use decimal fractions of a second, and may lose precision. For example
    the duration of 1 min 24 1/3 sec would be represented as `P0000-00-00T00:01:24.3333`.
 */
doremir_string_t doremir_time_to_iso(doremir_time_t time)
{
    doremir_time_t t = (doremir_time_t) time;
    string_t s = string("P0000-00");

    s = string_dappend(s, format_integer("-%02i", doremir_time_days(t)));
    s = string_dappend(s, format_integer("T%02i", doremir_time_hours(t)));
    s = string_dappend(s, format_integer(":%02i", doremir_time_minutes(t)));
    s = string_dappend(s, format_integer(":%02i", doremir_time_seconds(t)));
    // TODO approximate ratio
    s = string_dappend(s, string(".0000"));

    return s;
}


// --------------------------------------------------------------------------------

bool time_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return doremir_equal(x->value, y->value);
}

bool time_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return doremir_less_than(x->value, y->value);
}

bool time_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return doremir_greater_than(x->value, y->value);
}

doremir_ptr_t time_add(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return new_time(doremir_add(x->value, y->value));
}

doremir_ptr_t time_subtract(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return new_time(doremir_subtract(x->value, y->value));
}

doremir_ptr_t time_multiply(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return new_time(doremir_multiply(x->value, y->value));
}

doremir_ptr_t time_divide(doremir_ptr_t a, doremir_ptr_t b)
{
    doremir_time_t x = (doremir_time_t) a;
    doremir_time_t y = (doremir_time_t) b;
    return new_time(doremir_divide(x->value, y->value));
}

doremir_ptr_t time_absolute(doremir_ptr_t a)
{
    doremir_time_t x = (doremir_time_t) a;
    return new_time(doremir_absolute(x->value));
}

doremir_string_t time_show(doremir_ptr_t a)
{
    doremir_time_t t = (doremir_time_t) a;
    string_t s = string("<Time");

    s = string_dappend(s, format_integer(" %02id", doremir_time_days(t)));
    s = string_dappend(s, format_integer(" %02ih", doremir_time_hours(t)));
    s = string_dappend(s, format_integer(" %02im", doremir_time_minutes(t)));
    s = string_dappend(s, format_integer(" %02i+", doremir_time_seconds(t)));
    s = string_dappend(s, doremir_string_show(doremir_time_divisions(t)));
    s = string_dappend(s, string("s"));
    s = string_dappend(s, string(">"));
    return s;
}

doremir_ptr_t time_copy(doremir_ptr_t a)
{
    return doremir_time_copy(a);
}

void time_destroy(doremir_ptr_t a)
{
    doremir_time_destroy(a);
}

doremir_ptr_t time_impl(doremir_id_t interface)
{
    static doremir_equal_t time_equal_impl 
        = { time_equal };
    static doremir_order_t time_order_impl 
        = { time_less_than, time_greater_than };
    static doremir_string_show_t time_show_impl 
        = { time_show };
    static doremir_number_t  time_number_impl 
        = { time_add, time_subtract, time_multiply, time_divide, time_absolute };
    static doremir_copy_t time_copy_impl 
        = { time_copy };
    static doremir_destroy_t time_destroy_impl 
        = { time_destroy };

    
    switch (interface)
    {
    case doremir_equal_i:
        return &time_equal_impl;

    case doremir_order_i:
        return &time_order_impl;

    case doremir_string_show_i:
        return &time_show_impl;

    case doremir_number_i:
        return &time_number_impl;

    case doremir_copy_i:
        return &time_copy_impl;

    case doremir_destroy_i:
        return &time_destroy_impl;

    default:
        return NULL;
    }
}

