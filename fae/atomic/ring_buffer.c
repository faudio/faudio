
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/atomic/ring_buffer.h>
#include <fae/atomic.h>
#include <fae/buffer.h>
#include <fae/string.h>
#include <fae/util.h>

// http://www.cs.sunysb.edu/~skiena/392/programs/queue.c

struct _fae_atomic_ring_buffer_t {

    impl_t              impl;       //  Interface dispatcher
    buffer_t            data;       //  Data buffer

    atomic_t            start;
    atomic_t            stop;
};

fae_ptr_t atomic_ring_buffer_impl(fae_id_t interface);


/** Create a new ring buffer.
    @note
        O(n)
 */
fae_atomic_ring_buffer_t fae_atomic_ring_buffer_create(size_t size)
{
    ringbuffer_t b = fae_new(atomic_ring_buffer);

    b->impl = &atomic_ring_buffer_impl;
    b->data = fae_buffer_create(size);

    b->start = fae_atomic_create();
    b->stop  = fae_atomic_create();
    fae_atomic_set(b->start, 0);
    fae_atomic_set(b->stop, 0);

    return b;
}

/** Copy the given buffer.
    @note
        O(n)
 */
fae_atomic_ring_buffer_t
fae_atomic_ring_buffer_copy(fae_atomic_ring_buffer_t buffer)
{
    ringbuffer_t b = fae_new(atomic_ring_buffer);

    b->impl = &atomic_ring_buffer_impl;
    b->data = fae_buffer_copy(buffer->data);

    b->start = fae_atomic_create();
    b->stop  = fae_atomic_create();
    fae_atomic_set(b->start, fae_atomic_get(buffer->start));
    fae_atomic_set(b->stop, fae_atomic_get(buffer->stop));

    return b;
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
fae_atomic_ring_buffer_t
fae_atomic_ring_buffer_resize(size_t  size,
                              fae_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Swap the contents of the given ring buffers.
    @note
        O(n)
 */
void fae_atomic_ring_buffer_swap(fae_atomic_ring_buffer_t buffer,
                                 fae_atomic_ring_buffer_t buffer2)
{
    assert(false && "Not implemented");
}

/** Destroy the given ring buffer.
    @note
        O(n)
 */
void fae_atomic_ring_buffer_destroy(fae_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Return the size of the given ring buffer.
    @note
        O(n)
 */
size_t fae_atomic_ring_buffer_size(fae_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Read a value from the ring buffer.
    @note
        O(n)
 */
uint8_t fae_atomic_ring_buffer_read(fae_atomic_ring_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Write a value to the ring buffer.
    @note
        O(n)
 */
bool fae_atomic_ring_buffer_write(fae_atomic_ring_buffer_t buffer,
                                  uint8_t value)
{
    assert(false && "Not implemented");
}



// --------------------------------------------------------------------------------

fae_string_t atomic_ring_buffer_show(fae_ptr_t v)
{
    string_t s = string("<RingBuffer ");
    s = string_dappend(s, format_integral("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_ring_buffer_destroy(fae_ptr_t a)
{
    fae_atomic_ring_buffer_destroy(a);
}


fae_ptr_t atomic_ring_buffer_impl(fae_id_t interface)
{
    static fae_string_show_t atomic_ring_buffer_show_impl = { atomic_ring_buffer_show };
    static fae_destroy_t atomic_ring_buffer_destroy_impl = { atomic_ring_buffer_destroy };

    switch (interface) {
    case fae_string_show_i:
        return &atomic_ring_buffer_show_impl;

    case fae_destroy_i:
        return &atomic_ring_buffer_destroy_impl;

    default:
        return NULL;
    }
}

