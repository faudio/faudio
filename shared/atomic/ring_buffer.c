
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

struct _fa_atomic_ring_buffer_t {

    impl_t              impl;       //  Interface dispatcher
    buffer_t            data;       //  Data buffer

    atomic_t            start;
    atomic_t            stop;
};

fa_ptr_t atomic_ring_buffer_impl(fa_id_t interface);


/** Create a new ring buffer.
    @note
        O(n)
 */
fa_atomic_ring_buffer_t fa_atomic_ring_buffer_create(size_t size)
{
    ringbuffer_t b = fa_new(atomic_ring_buffer);

    b->impl = &atomic_ring_buffer_impl;
    b->data = fa_buffer_create(size);

    b->start = fa_atomic_create();
    b->stop  = fa_atomic_create();
    fa_atomic_set(b->start, 0);
    fa_atomic_set(b->stop, 0);

    return b;
}

/** Copy the given buffer.
    @note
        O(n)
 */
fa_atomic_ring_buffer_t
fa_atomic_ring_buffer_copy(fa_atomic_ring_buffer_t buffer)
{
    ringbuffer_t b = fa_new(atomic_ring_buffer);

    b->impl = &atomic_ring_buffer_impl;
    b->data = fa_buffer_copy(buffer->data);

    b->start = fa_atomic_create();
    b->stop  = fa_atomic_create();
    fa_atomic_set(b->start, fa_atomic_get(buffer->start));
    fa_atomic_set(b->stop, fa_atomic_get(buffer->stop));

    return b;
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
fa_atomic_ring_buffer_t
fa_atomic_ring_buffer_resize(size_t  size,
                             fa_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Swap the contents of the given ring buffers.
    @note
        O(n)
 */
void fa_atomic_ring_buffer_swap(fa_atomic_ring_buffer_t buffer,
                                fa_atomic_ring_buffer_t buffer2)
{
    assert(false && "Not implemented");
}

/** Destroy the given ring buffer.
    @note
        O(n)
 */
void fa_atomic_ring_buffer_destroy(fa_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Return the size of the given ring buffer.
    @note
        O(n)
 */
size_t fa_atomic_ring_buffer_size(fa_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Read a value from the ring buffer.
    @note
        O(n)
 */
uint8_t fa_atomic_ring_buffer_read(fa_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Write a value to the ring buffer.
    @note
        O(n)
 */
bool fa_atomic_ring_buffer_write(fa_atomic_ring_buffer_t buffer,
                                 uint8_t value)
{
    assert(false && "Not implemented");
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

