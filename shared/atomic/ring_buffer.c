
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/atomic/ring_buffer.h>
#include <fa/atomic.h>
#include <fa/buffer.h>
#include <fa/string.h>
#include <fa/util.h>

// http://www.cs.sunysb.edu/~skiena/392/programs/queue.c
typedef fa_atomic_ring_buffer_t ring_buffer_t;

struct _fa_atomic_ring_buffer_t {

    impl_t              impl;                   //  Interface dispatcher
    
    ptr_t               data;                   //  Underlying buffer
    size_t              size;
    size_t              first, last, count;
};


/** Create a new ring buffer.
    @note
        O(n)
 */
ring_buffer_t fa_atomic_ring_buffer_create(size_t size)
{
    ringbuffer_t b = fa_new(atomic_ring_buffer);

    fa_ptr_t atomic_ring_buffer_impl(fa_id_t interface);
    b->impl = &atomic_ring_buffer_impl;

    b->data = fa_malloc(size/*+1*/);    // Memory region (data..data+size) [0,1,2,3,4,5]
    b->size = size;

    b->first = 0;                   // Next read, always < size
    b->last  = size-1;              // One before next write, always < size
    b->count = 0;                   // Bytes written not yet read

    return b;
}

ring_buffer_t
fa_atomic_ring_buffer_resize(size_t         size,
                             ring_buffer_t  buffer)
{
    assert(false && "Not implemented");
}


ring_buffer_t
fa_atomic_ring_buffer_copy(ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

void fa_atomic_ring_buffer_destroy(ring_buffer_t buffer)
{
    // fa_destroy(buffer->data);
    fa_free(buffer->data);
    fa_delete(buffer);
}


size_t fa_atomic_ring_buffer_size(ring_buffer_t buffer)
{
    return buffer->size;
}

uint8_t fa_atomic_ring_buffer_read(ring_buffer_t buffer)
{
    char x;
    if (buffer->count <= 0)
        assert(false && "Underflow");
    else {
        x = ((char*) buffer->data)[buffer->first];
        buffer->first = (buffer->first+1) % buffer->size;
        buffer->count = buffer->count - 1;
    }
    return x;
}

bool fa_atomic_ring_buffer_write(ring_buffer_t buffer,
                                 uint8_t value)
{
    if (buffer->count >= buffer->size)
        assert(false && "Overflow");
    else {
        buffer->last = (buffer->last+1) % buffer->size;
        ((char*) buffer->data)[buffer->last] = value;
        buffer->count = buffer->count + 1;
    }
    return true;
}



// --------------------------------------------------------------------------------

fa_string_t atomic_ring_buffer_show(fa_ptr_t v)
{
    string_t s = string("<RingBuffer ");
    s = string_dappend(s, format_integral("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_ring_buffer_destroy(fa_ptr_t a)
{
    fa_atomic_ring_buffer_destroy(a);
}


fa_ptr_t atomic_ring_buffer_impl(fa_id_t interface)
{
    static fa_string_show_t atomic_ring_buffer_show_impl = { atomic_ring_buffer_show };
    static fa_destroy_t atomic_ring_buffer_destroy_impl = { atomic_ring_buffer_destroy };

    switch (interface) {
    case fa_string_show_i:
        return &atomic_ring_buffer_show_impl;

    case fa_destroy_i:
        return &atomic_ring_buffer_destroy_impl;

    default:
        return NULL;
    }
}

