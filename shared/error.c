
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa.h>
#include <fa/string.h>
#include <fa/error.h>
#include <fa/util.h>

typedef fa_error_interface_t error_interface_t;

struct simple_error {
    fa_impl_t              impl;           //  Interface dispatcher
    fa_error_severity_t    severity;
    fa_string_t            message;
    fa_string_t            origin;
};

typedef struct simple_error       *simple_fa_error_t;


void fa_log(fa_ptr_t data, fa_error_t error);

fa_error_t fa_error_create_simple(
    fa_error_severity_t    severity,
    fa_string_t            message,
    fa_string_t            origin
)
{
    fa_ptr_t simple_error_impl(fa_id_t interface);
    simple_fa_error_t e  = fa_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = severity;
    e->message  = message;
    e->origin   = origin;
    return (fa_error_t) e;
}

fa_error_t fa_error_copy_simple(simple_fa_error_t simple)
{
    fa_ptr_t simple_error_impl(fa_id_t interface);
    simple_fa_error_t e  = fa_new_struct(simple_error);
    e->impl     = &simple_error_impl;
    e->severity = simple->severity;
    e->message  = fa_copy(simple->message);
    e->origin   = simple->origin ? fa_copy(simple->origin) : NULL;
    return (fa_error_t) e;
}

void fa_error_destroy_simple(simple_fa_error_t simple)
{
    if (simple->message)
        fa_destroy(simple->message);
    if (simple->origin)
		fa_destroy(simple->origin);
    fa_delete(simple);
}


fa_error_severity_t fa_error_severity(fa_error_t a)
{
    assert(fa_interface(fa_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fa_interface(fa_error_i, a))->severity(a);
}

fa_string_t fa_error_message(fa_error_t a)
{
    assert(fa_interface(fa_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fa_interface(fa_error_i, a))->message(a);
}

fa_string_t fa_error_origin(fa_error_t a)
{
    assert(fa_interface(fa_error_i, a) && "Must implement Error");
    return ((error_interface_t *) fa_interface(fa_error_i, a))->origin(a);
}

bool fa_error_check(fa_ptr_t a)
{
    if (a == NULL) {
        return false;
    }

    return fa_interface(fa_error_i, a);
}

void fa_error_log(fa_ptr_t context, fa_error_t error)
{
    fa_log(context, error);
}

fa_string_t fa_error_format(bool colored, fa_error_t a)
{
    simple_fa_error_t simple = (simple_fa_error_t) a;
    fa_string_t str = fa_string("");

    switch (simple->severity) {
    case info:
        str = fa_string_dappend(str, colored ? fa_string("\x1b[32m[INFO]\x1b[0m    ") : fa_string("[INFO]    "));
        break;

    case warning:
        str = fa_string_dappend(str, colored ? fa_string("\x1b[33m[WARNING]\x1b[0m ") : fa_string("[WARNING] "));
        break;

    case error:
        str = fa_string_dappend(str, colored ? fa_string("\x1b[31m[ERROR]\x1b[0m   ") : fa_string("[ERROR]   "));
        break;

    case misc:
        str = fa_string_dappend(str, colored ? fa_string("\x1b[35m[MISC]\x1b[0m    ") : fa_string("[MISC]    "));
        break;

    default:
        assert(false && "Missing label");
    }

    if (simple->origin && (fa_string_length(simple->origin) > 0)) {
        if (colored)
			str = fa_string_dappend(str, fa_string("\x1b[36m"));
        str = fa_string_dappend(str, fa_copy(simple->origin));
        str = fa_string_dappend(str, colored ? fa_string(":\x1b[0m ") : fa_string(": "));
    }

    if (simple->message) {
        str = fa_string_dappend(str, fa_copy(simple->message));
    } else {
        str = fa_string_dappend(str, fa_string("[NULL]"));
    }

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
    simple_fa_error_t simple = (simple_fa_error_t) a;
    return simple->severity;
}

fa_string_t simple_error_message(fa_ptr_t a)
{
    simple_fa_error_t simple = (simple_fa_error_t) a;
    return simple->message;
}

fa_string_t simple_error_origin(fa_ptr_t a)
{
    simple_fa_error_t simple = (simple_fa_error_t) a;
    return simple->origin;
}

fa_string_t simple_error_show(fa_ptr_t a)
{
    simple_fa_error_t simple = (simple_fa_error_t) a;
    fa_string_t result = fa_string("<");

    switch (simple->severity) {
    case info:
        result = fa_string_dappend(result, fa_string("Info "));
        break;

    case warning:
        result = fa_string_dappend(result, fa_string("Warning "));
        break;

    case error:
        result = fa_string_dappend(result, fa_string("Error "));
        break;

    case misc:
        result = fa_string_dappend(result, fa_string("Misc "));
        break;

    default:
        assert(false && "Missing label");
    }

    if (simple->origin && (fa_string_length(simple->origin) > 0)) {
        result = fa_string_dappend(result, fa_copy(simple->origin));
        result = fa_string_dappend(result, fa_string(": "));
    }

    result = fa_string_dappend(result, fa_copy(simple->message));
    result = fa_string_dappend(result, fa_string(">"));

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

