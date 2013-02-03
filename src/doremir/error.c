
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir.h>
#include <doremir/string.h>
#include <doremir/error.h>
#include <doremir/util.h>

typedef doremir_error_interface_t error_interface_t;

struct simple_error {
    impl_t        impl;       //  Interface dispatcher
    severity_t    severity;
    string_t      message;
    string_t      origin;
};

typedef struct simple_error    *   simple_error_t;

void doremir_audio_engine_log(doremir_ptr_t ct, doremir_error_t e);


// --------------------------------------------------------------------------------

/** Creates a simple error.
    @param severity     Severity of the error.
    @param message      Error message.
    @param origin       Error origin (typically module name).
    @return
        A value of some type implementing
            [Error](@ref doremir_error_interface_t),
            [Copy](@ref doremir_copy_t) and
            [Destroy](@ref doremir_destroy_t)
 */
doremir_error_t doremir_error_create_simple(
    doremir_error_severity_t    severity,
    doremir_string_t            message,
    doremir_string_t            origin
)
{
    doremir_ptr_t simple_error_impl(doremir_id_t interface);
    simple_error_t e  = doremir_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = severity;
    e->message  = message;
    e->origin   = origin;
    return (error_t) e;
}

doremir_error_t doremir_error_copy_simple(simple_error_t simple)
{
    doremir_ptr_t simple_error_impl(doremir_id_t interface);
    simple_error_t e  = doremir_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = simple->severity;
    e->message  = doremir_copy(simple->message);
    e->origin   = doremir_copy(simple->origin);
    return (error_t) e;
}

void doremir_error_destroy_simple(simple_error_t simple)
{
    doremir_destroy(simple->message);
    doremir_destroy(simple->origin);
    doremir_delete(simple);
}


/** Return the severity of the given error.
 */
doremir_error_severity_t doremir_error_severity(doremir_error_t a)
{                                                                      
    assert(doremir_interface(doremir_error_i, a) && "Must implement Error");
    return ((error_interface_t *) doremir_interface(doremir_error_i, a))->severity(a);
}

/** Return the message of the given error.
 */
doremir_string_t doremir_error_message(doremir_error_t a)
{
    assert(doremir_interface(doremir_error_i, a) && "Must implement Error");
    return ((error_interface_t *) doremir_interface(doremir_error_i, a))->message(a);
}

/** Return the origin of the given error.
 */
doremir_string_t doremir_error_origin(doremir_error_t a)
{
    assert(doremir_interface(doremir_error_i, a) && "Must implement Error");
    return ((error_interface_t *) doremir_interface(doremir_error_i, a))->origin(a);
}

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref doremir_error_log) as in:

    ~~~
    if (doremir_check(value)) {
        doremir_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
 */
bool doremir_error_check(doremir_ptr_t a)
{
    return doremir_interface(doremir_error_i, a);
}

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref doremir_error_interface_t).
 */
void doremir_error_log(doremir_ptr_t context, doremir_error_t error)
{
    doremir_audio_engine_log(context, error);
}

/** Convert the given error to a formated string.
    @param colored Include color escapes for terminals.
 */
doremir_string_t doremir_error_format(bool colored, doremir_error_t a)
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

    if (doremir_string_length(simple->origin) > 0) {
        str = string_dappend(str, strs[4 + colored * 6]);
        str = string_dappend(str, doremir_copy(simple->origin));
        str = string_dappend(str, strs[5 + colored * 6]);
    }

    str = string_dappend(str, doremir_copy(simple->message));

    return str;
}


// --------------------------------------------------------------------------------

doremir_ptr_t simple_error_copy(doremir_ptr_t a)
{
    return doremir_error_copy_simple(a);
}

void simple_error_destroy(doremir_ptr_t a)
{
    doremir_error_destroy_simple(a);
}

doremir_error_severity_t simple_error_severity(doremir_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->severity;
}

doremir_string_t simple_error_message(doremir_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->message;
}

doremir_string_t simple_error_origin(doremir_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->origin;
}

doremir_string_t simple_error_show(doremir_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    string_t str = string("<");

    switch (simple->severity) {
        case info:
            str = string_dappend(str, string("Info "));
            break;

        case warning:
            str = string_dappend(str, string("Warning "));
            break;

        case error:
            str = string_dappend(str, string("Error "));
            break;

        case misc:
            str = string_dappend(str, string("Misc "));
            break;

        default:
            assert(false && "Missing label");
    }

    if (doremir_string_length(simple->origin) > 0) {
        str = string_dappend(str, doremir_copy(simple->origin));
        str = string_dappend(str, string(": "));
    }

    str = string_dappend(str, doremir_copy(simple->message));
    str = string_dappend(str, string(">"));

    return str;
}

doremir_ptr_t simple_error_impl(doremir_id_t interface)
{
    static doremir_string_show_t simple_error_show_impl = { simple_error_show };
    static doremir_copy_t simple_error_copy_impl = { simple_error_copy };
    static doremir_destroy_t simple_error_destroy_impl = { simple_error_destroy };
    static doremir_error_interface_t simple_error_error_impl =
    { simple_error_severity, simple_error_message, simple_error_origin };

    switch (interface) {
        case doremir_copy_i:
            return &simple_error_copy_impl;

        case doremir_destroy_i:
            return &simple_error_destroy_impl;

        case doremir_error_i:
            return &simple_error_error_impl;

        case doremir_string_show_i:
            return &simple_error_show_impl;

        default:
            return NULL;
    }
}

