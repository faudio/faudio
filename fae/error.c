
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa.h>
#include <fa/string.h>
#include <fa/error.h>
#include <fa/util.h>

typedef fa_error_interface_t error_interface_t;

struct simple_error {
    impl_t        impl;       //  Interface dispatcher
    severity_t    severity;
    string_t      message;
    string_t      origin;
};

typedef struct simple_error       *simple_error_t;


void fa_fa_log(fa_ptr_t data, fa_error_t error);

/** Creates a simple error.
    @param severity     Severity of the error.
    @param message      Error message.
    @param origin       Error origin (typically module name).
    @return
        A value of some type implementing
            [Error](@ref fa_error_interface_t),
            [Copy](@ref fa_copy_t) and
            [Destroy](@ref fa_destroy_t)
 */
fa_error_t fa_error_create_simple(
    fa_error_severity_t    severity,
    fa_string_t            message,
    fa_string_t            origin
)
{
    fa_ptr_t simple_error_impl(fa_id_t interface);
    simple_error_t e  = fa_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = severity;
    e->message  = fa_copy(message);
    e->origin   = fa_copy(origin);
    return (error_t) e;
}

fa_error_t fa_error_copy_simple(simple_error_t simple)
{
    fa_ptr_t simple_error_impl(fa_id_t interface);
    simple_error_t e  = fa_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = simple->severity;
    e->message  = fa_copy(simple->message);
    e->origin   = fa_copy(simple->origin);
    return (error_t) e;
}

void fa_error_destroy_simple(simple_error_t simple)
{
    fa_destroy(simple->message);
    fa_destroy(simple->origin);
    fa_delete(simple);
}


/** Return the severity of the given error.
 */
fa_error_severity_t fa_error_severity(fa_error_t a)
{
    assert(fa_interface(fa_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fa_interface(fa_error_i, a))->severity(a);
}

/** Return the message of the given error.
 */
fa_string_t fa_error_message(fa_error_t a)
{
    assert(fa_interface(fa_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fa_interface(fa_error_i, a))->message(a);
}

/** Return the origin of the given error.
 */
fa_string_t fa_error_origin(fa_error_t a)
{
    assert(fa_interface(fa_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fa_interface(fa_error_i, a))->origin(a);
}

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref fa_error_log) as in:

    ~~~
    if (fa_check(value)) {
        fa_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
 */
bool fa_error_check(fa_ptr_t a)
{
    return fa_interface(fa_error_i, a);
}

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref fa_error_interface_t).
 */
void fa_error_log(fa_ptr_t context, fa_error_t error)
{
    fa_fa_log(context, error);
}

/** Convert the given error to a formated string.
    @param colored Include color escapes for terminals.
 */
fa_string_t fa_error_format(bool colored, fa_error_t a)
{
    simple_error_t simple = (simple_error_t) a;
    string_t str = string("");

    string_t strs[12] = {
        string("[INFO]    "),
        string("[WARNING] "),
        string("[ERROR]   "),
        string("[MISC]    "),
        string(""),
        string(": "),

        string("\x1b[32m[INFO]\x1b[0m    "),
        string("\x1b[33m[WARNING]\x1b[0m "),
        string("\x1b[31m[ERROR]\x1b[0m   "),
        string("\x1b[35m[MISC]\x1b[0m    "),
        string("\x1b[36m"),
        string(":\x1b[0m ")
    };

    switch (simple->severity) {
    case info:
        str = string_dappend(str, strs[0 + colored * 6]);
        break;

    case warning:
        str = string_dappend(str, strs[1 + colored * 6]);
        break;

    case error:
        str = string_dappend(str, strs[2 + colored * 6]);
        break;

    case misc:
        str = string_dappend(str, strs[3 + colored * 6]);
        break;

    default:
        assert(false && "Missing label");
    }

    if (fa_string_length(simple->origin) > 0) {
        str = string_dappend(str, strs[4 + colored * 6]);
        str = string_dappend(str, fa_copy(simple->origin));
        str = string_dappend(str, strs[5 + colored * 6]);
    }

    str = string_dappend(str, fa_copy(simple->message));

    return str;
}


// --------------------------------------------------------------------------------

fa_ptr_t simple_error_copy(fa_ptr_t a)
{
    return fa_error_copy_simple(a);
}

void simple_error_destroy(fa_ptr_t a)
{
    fa_error_destroy_simple(a);
}

fa_error_severity_t simple_error_severity(fa_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->severity;
}

fa_string_t simple_error_message(fa_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->message;
}

fa_string_t simple_error_origin(fa_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->origin;
}

fa_string_t simple_error_show(fa_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    string_t result = string("<");

    switch (simple->severity) {
    case info:
        result = string_dappend(result, string("Info "));
        break;

    case warning:
        result = string_dappend(result, string("Warning "));
        break;

    case error:
        result = string_dappend(result, string("Error "));
        break;

    case misc:
        result = string_dappend(result, string("Misc "));
        break;

    default:
        assert(false && "Missing label");
    }

    if (fa_string_length(simple->origin) > 0) {
        result = string_dappend(result, fa_copy(simple->origin));
        result = string_dappend(result, string(": "));
    }

    result = string_dappend(result, fa_copy(simple->message));
    result = string_dappend(result, string(">"));

    return result;
}

fa_ptr_t simple_error_impl(fa_id_t interface)
{
    static fa_string_show_t simple_error_show_impl
        = { simple_error_show };
    static fa_copy_t simple_error_copy_impl
        = { simple_error_copy };
    static fa_destroy_t simple_error_destroy_impl
        = { simple_error_destroy };
    static fa_error_interface_t simple_error_error_impl
        = { simple_error_severity, simple_error_message, simple_error_origin };

    switch (interface) {
    case fa_copy_i:
        return &simple_error_copy_impl;

    case fa_destroy_i:
        return &simple_error_destroy_impl;

    case fa_error_i:
        return &simple_error_error_impl;

    case fa_string_show_i:
        return &simple_error_show_impl;

    default:
        return NULL;
    }
}

