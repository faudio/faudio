
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic/ring_buffer.h>
#include <doremir/atomic.h>
#include <doremir/buffer.h>
#include <doremir/string.h>
#include <doremir/util.h>

struct _doremir_atomic_ring_buffer_t {

        doremir_impl_t      impl;       /* Interface dispatcher */

        buffer_t            data;       /* Actual data */

        atomic_t            start;
        atomic_t            stop;
    };

doremir_ptr_t atomic_ring_buffer_impl(doremir_id_t interface);


/** Create a new ring buffer.
    @note
        O(n)
 */
doremir_atomic_ring_buffer_t doremir_atomic_ring_buffer_create(size_t size)
{
    ringbuffer_t b = doremir_new(atomic_ring_buffer);

    b->impl = &atomic_ring_buffer_impl;
    b->data = doremir_buffer_create(size);

    b->start = doremir_atomic_create();
    b->stop  = doremir_atomic_create();
    doremir_atomic_set(b->start, 0);
    doremir_atomic_set(b->stop, 0);

    return b;
}

/** Copy the given buffer.
    @note
        O(n)
 */
doremir_atomic_ring_buffer_t 
doremir_atomic_ring_buffer_copy(doremir_atomic_ring_buffer_t buffer)
{
    ringbuffer_t b = doremir_new(atomic_ring_buffer);

    b->impl = &atomic_ring_buffer_impl;
    b->data = doremir_buffer_copy(buffer->data);

    b->start = doremir_atomic_create();
    b->stop  = doremir_atomic_create();
    doremir_atomic_set(b->start, doremir_atomic_get(buffer->start));
    doremir_atomic_set(b->stop, doremir_atomic_get(buffer->stop));

    return b;
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
doremir_atomic_ring_buffer_t 
doremir_atomic_ring_buffer_resize(size_t  size,
                                  doremir_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Swap the contents of the given ring buffers.
    @note
        O(n)
 */
void doremir_atomic_ring_buffer_swap(doremir_atomic_ring_buffer_t buffer,
                                     doremir_atomic_ring_buffer_t buffer2)
{
    assert(false && "Not implemented");
}

/** Destroy the given ring buffer.
    @note
        O(n)
 */
void doremir_atomic_ring_buffer_destroy(doremir_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Return the size of the given ring buffer.
    @note
        O(n)
 */
size_t doremir_atomic_ring_buffer_size(doremir_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Read a value from the ring buffer.
    @note
        O(n)
 */
uint8_t doremir_atomic_ring_buffer_read(doremir_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Write a value to the ring buffer.
    @note
        O(n)
 */
bool doremir_atomic_ring_buffer_write(doremir_atomic_ring_buffer_t buffer,
                                      uint8_t value)
{
    assert(false && "Not implemented");
}



// --------------------------------------------------------------------------------

doremir_string_t atomic_ring_buffer_show(doremir_ptr_t v)
{
    string_t s = string("<RingBuffer ");
    s = string_dappend(s, format_int("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_ring_buffer_destroy(doremir_ptr_t a)
{
    doremir_atomic_ring_buffer_destroy(a);
}


doremir_ptr_t atomic_ring_buffer_impl(doremir_id_t interface)
{
    static doremir_string_show_t atomic_ring_buffer_show_impl = { atomic_ring_buffer_show };
    static doremir_destroy_t atomic_ring_buffer_destroy_impl = { atomic_ring_buffer_destroy };

    switch (interface)
    {
    case doremir_string_show_i:
        return &atomic_ring_buffer_show_impl;

    case doremir_destroy_i:
        return &atomic_ring_buffer_destroy_impl;

    default:
        return NULL;
    }
}

