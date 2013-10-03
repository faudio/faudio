
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

    b->data = fa_malloc(size/*+1*/);    // Memory region (data..data+size) [0,1,2,3,4]
    b->size = size;

    b->first = 0;                   // Next read, always < size
    b->last  = 0;                   // Next write, always < size
    b->count = 0;                   // Bytes written not yet read, always <= size

    return b;
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
        ((char*) buffer->data)[buffer->last] = value;
        buffer->last = (buffer->last+1) % buffer->size;
        buffer->count = buffer->count + 1;
    }
    return true;
}

// TODO above methods (unsafe) should be primitive
// Also they should put check *inside* assertion

// TODO more efficient versions
// Can using memcpy + slicing or virtual memory

size_t fa_atomic_ring_buffer_read_many(uint8_t* dst,
                                       ring_buffer_t src,
                                       size_t count)
{             
    for (size_t i = 0; i < count; ++i) {
        dst[i] = fa_atomic_ring_buffer_read(src);
    }
    return count;
}

size_t fa_atomic_ring_buffer_write_many(ring_buffer_t dst,
                                        uint8_t* src,
                                        size_t count)
{
    for (size_t i = 0; i < count; ++i) {
        fa_atomic_ring_buffer_write(dst, src[i]);
    }
    return count;
}


float fa_atomic_ring_buffer_read_float(ring_buffer_t buffer)
{
    float x;
    fa_atomic_ring_buffer_read_many((uint8_t*) &x, buffer, sizeof(x));
    return x;
}


double fa_atomic_ring_buffer_read_double(ring_buffer_t  buffer)
{
    double x;
    fa_atomic_ring_buffer_read_many((uint8_t*) &x, buffer, sizeof(x));
    return x;
}


bool fa_atomic_ring_buffer_write_float(ring_buffer_t buffer,
                                       float value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (uint8_t*) &value, sizeof(value));
}

bool fa_atomic_ring_buffer_write_double(ring_buffer_t buffer,
                                       double value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (uint8_t*) &value, sizeof(value));
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

