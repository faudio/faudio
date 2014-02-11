
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/io.h>
#include <fa/util.h>

#define kMaxSize (1024*16)

struct endian_filter {
    bool closed;
    size_t count;
    char buffer[kMaxSize];
};

// #define endian_printf printf
#define endian_printf(fmt, ...) // do nothing

static inline
void prepare(fa_ptr_t x)
{
    struct endian_filter *filter = (struct endian_filter *) x;

    filter->count = 0;
    filter->closed = false;
    mark_used(filter);
}

static inline
void push(fa_ptr_t x, fa_buffer_t buffer)
{
    struct endian_filter *filter = (struct endian_filter *) x;

    if (!buffer) {
        filter->closed = true;
    } else {
        size_t size = fa_buffer_size(buffer);
        void*  raw  = fa_buffer_unsafe_address(buffer);

        // Copy size bytes to buffer
        assert(filter->count + size < kMaxSize && "Overflow in endian buffer");
        memcpy(&filter->buffer[filter->count], raw, size);
        filter->count += size;
    }
    mark_used(filter);
}

static inline
void pull(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    struct endian_filter *filter = (struct endian_filter *) x;
    if (filter->closed) {
        cb(data, NULL);
    } else {
        // TODO wait for 8 bytes
        fa_with_temp(transient, fa_buffer_wrap(filter->buffer, filter->count, NULL, NULL)) {
            cb(data, transient);            
            filter->count = 0;
        }
    }

    mark_used(filter);
}


fa_io_filter_t fa_io_create_endian_filter()
{
    struct filter *filter = fa_new_struct(endian_filter);
    prepare(filter);
    // TODO free filter
    return fa_io_create_simple_filter(push, pull, filter);
}