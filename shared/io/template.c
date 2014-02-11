
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/io.h>
#include <fa/util.h>

struct T_filter {
    size_t count;
    char buffer[256];
};

// #define T_printf printf
#define T_printf(fmt, ...) // do nothing

static inline
void prepare(fa_ptr_t x)
{
    struct T_filter *filter = (struct T_filter *) x;
    mark_used(filter);
}

static inline
void push(fa_ptr_t x, fa_buffer_t buffer)
{
    struct T_filter *filter = (struct T_filter *) x;
    mark_used(filter);
}

static inline
void pull(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    struct T_filter *filter = (struct T_filter *) x;
    mark_used(filter);
}


fa_io_filter_t fa_io_create_T_filter()
{
    struct filter *filter = fa_new_struct(T_filter);
    prepare(filter);
    // TODO free filter
    return fa_io_create_simple_filter(push, pull, filter);
}