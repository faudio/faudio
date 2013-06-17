
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae.h>
#include <fae/string.h>
#include <fae/error.h>
#include <fae/util.h>

typedef fae_error_interface_t error_interface_t;

struct simple_error {
    impl_t        impl;       //  Interface dispatcher
    severity_t    severity;
    string_t      message;
    string_t      origin;
};

typedef struct simple_error       *simple_error_t;


void fae_audio_engine_log(fae_ptr_t data, fae_error_t error);

/** Creates a simple error.
    @param severity     Severity of the error.
    @param message      Error message.
    @param origin       Error origin (typically module name).
    @return
        A value of some type implementing
            [Error](@ref fae_error_interface_t),
            [Copy](@ref fae_copy_t) and
            [Destroy](@ref fae_destroy_t)
 */
fae_error_t fae_error_create_simple(
    fae_error_severity_t    severity,
    fae_string_t            message,
    fae_string_t            origin
)
{
    fae_ptr_t simple_error_impl(fae_id_t interface);
    simple_error_t e  = fae_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = severity;
    e->message  = fae_copy(message);
    e->origin   = fae_copy(origin);
    return (error_t) e;
}

fae_error_t fae_error_copy_simple(simple_error_t simple)
{
    fae_ptr_t simple_error_impl(fae_id_t interface);
    simple_error_t e  = fae_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = simple->severity;
    e->message  = fae_copy(simple->message);
    e->origin   = fae_copy(simple->origin);
    return (error_t) e;
}

void fae_error_destroy_simple(simple_error_t simple)
{
    fae_destroy(simple->message);
    fae_destroy(simple->origin);
    fae_delete(simple);
}


/** Return the severity of the given error.
 */
fae_error_severity_t fae_error_severity(fae_error_t a)
{
    assert(fae_interface(fae_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fae_interface(fae_error_i, a))->severity(a);
}

/** Return the message of the given error.
 */
fae_string_t fae_error_message(fae_error_t a)
{
    assert(fae_interface(fae_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fae_interface(fae_error_i, a))->message(a);
}

/** Return the origin of the given error.
 */
fae_string_t fae_error_origin(fae_error_t a)
{
    assert(fae_interface(fae_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fae_interface(fae_error_i, a))->origin(a);
}

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref fae_error_log) as in:

    ~~~
    if (fae_check(value)) {
        fae_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
 */
bool fae_error_check(fae_ptr_t a)
{
    return fae_interface(fae_error_i, a);
}

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref fae_error_interface_t).
 */
void fae_error_log(fae_ptr_t context, fae_error_t error)
{
    fae_audio_engine_log(context, error);
}

/** Convert the given error to a formated string.
    @param colored Include color escapes for terminals.
 */
fae_string_t fae_error_format(bool colored, fae_error_t a)
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

    if (fae_string_length(simple->origin) > 0) {
        str = string_dappend(str, strs[4 + colored * 6]);
        str = string_dappend(str, fae_copy(simple->origin));
        str = string_dappend(str, strs[5 + colored * 6]);
    }

    str = string_dappend(str, fae_copy(simple->message));

    return str;
}


// --------------------------------------------------------------------------------

fae_ptr_t simple_error_copy(fae_ptr_t a)
{
    return fae_error_copy_simple(a);
}

void simple_error_destroy(fae_ptr_t a)
{
    fae_error_destroy_simple(a);
}

fae_error_severity_t simple_error_severity(fae_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->severity;
}

fae_string_t simple_error_message(fae_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->message;
}

fae_string_t simple_error_origin(fae_ptr_t a)
{
    simple_error_t simple = (simple_error_t) a;
    return simple->origin;
}

fae_string_t simple_error_show(fae_ptr_t a)
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

    if (fae_string_length(simple->origin) > 0) {
        str = string_dappend(str, fae_copy(simple->origin));
        str = string_dappend(str, string(": "));
    }

    str = string_dappend(str, fae_copy(simple->message));
    str = string_dappend(str, string(">"));

    return str;
}

fae_ptr_t simple_error_impl(fae_id_t interface)
{
    static fae_string_show_t simple_error_show_impl = { simple_error_show };
    static fae_copy_t simple_error_copy_impl = { simple_error_copy };
    static fae_destroy_t simple_error_destroy_impl = { simple_error_destroy };
    static fae_error_interface_t simple_error_error_impl =
    { simple_error_severity, simple_error_message, simple_error_origin };

    switch (interface) {
    case fae_copy_i:
        return &simple_error_copy_impl;

    case fae_destroy_i:
        return &simple_error_destroy_impl;

    case fae_error_i:
        return &simple_error_error_impl;

    case fae_string_show_i:
        return &simple_error_show_impl;

    default:
        return NULL;
    }
}

