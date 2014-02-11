
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/atomic/ring_buffer.h>
#include <fa/atomic.h>
#include <fa/buffer.h>
#include <fa/string.h>
#include <fa/util.h>

/**
    Single-read/single-write implementation.
    
    Implementation defined by the following methods:
    
        - size
        - remaining
        - canRead
        - canWrite
        - unsafeReadByte
        - unsafeWriteByte

    Both unsafe methods will fail with assertions if used when canRead or canWrite would return false.
    
 */

typedef uint8_t byte_t;
typedef fa_atomic_ring_buffer_t ring_buffer_t;

struct _fa_atomic_ring_buffer_t {

    impl_t              impl;                   //  Interface dispatcher

    size_t              size;                   //  Size (immutable)
    size_t              first, last;            //  Next read or write, always < size
    atomic_t            count;                  //  Bytes written not yet read, always <= size

    byte_t             *data;                   //  Memory region (data..data+size) [0,1,2,3,4] 

    bool                closed;
    enum { 
        buffer_overflowed, 
        buffer_alright, 
        buffer_underflowed 
    }                   status;                 //  This is used to prevent too many error messages
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

    b->data = fa_malloc(size/*+1*/);    
    b->size = size;

    b->first = 0;                   
    b->last  = 0;
    b->count = atomic();

    // if count == size, the buffer is full
    // if count == 0,    the buffer is empty
    // we can always read n bytes, where n == count
    // we can always write n bytes, where n == (size-count)

    b->closed = false;
    b->status = buffer_alright;

    return b;
}

#define get_size(A) ((size_t) fa_atomic_get(A))   

void fa_atomic_ring_buffer_destroy(ring_buffer_t buffer)
{
    fa_destroy(buffer->count);
    fa_free(buffer->data);
    
    fa_delete(buffer);
}


size_t fa_atomic_ring_buffer_size(ring_buffer_t buffer)
{
    return buffer->size;
}

size_t fa_atomic_ring_buffer_remaining(ring_buffer_t buffer)
{
    return get_size(buffer->count);
}
double fa_atomic_ring_buffer_filled(ring_buffer_t buffer)
{
    return (double) get_size(buffer->count) / (double) buffer->size;
}

bool fa_atomic_ring_buffer_can_read(ring_buffer_t buffer, size_t n)
{
    return get_size(buffer->count) >= n;
}

bool fa_atomic_ring_buffer_can_write(ring_buffer_t buffer, size_t n)
{
    return (get_size(buffer->count) + n) <= buffer->size;
}

void fa_atomic_ring_buffer_close(ring_buffer_t buffer)
{
    buffer->closed = true;
}
bool fa_atomic_ring_buffer_is_closed(ring_buffer_t buffer)
{
    return buffer->closed;
}


byte_t unsafe_read_byte(ring_buffer_t buffer)
{
    byte_t x;
    assert(fa_atomic_ring_buffer_can_read(buffer, 1) && "Underflow");
    {
        x = buffer->data[buffer->first];
        buffer->first = (buffer->first + 1) % buffer->size;
        // buffer->count = buffer->count - 1;   
        
        // DEBUG
        // OSAtomicAdd32Barrier((int32_t) (-1), (int32_t*) &buffer->count);
        fa_atomic_add(buffer->count, -1);
    }
    // if (x == 0) {
    //     printf("size: %d\n",    (int) buffer->size);
    //     printf("first: %d\n",   (int) buffer->first);
    //     printf("last: %d\n",    (int) buffer->last);
    //     printf("count: %d\n",   (int) buffer->count);
    //     assert(false);
    //  }
    return x;
}

bool unsafe_write_byte(ring_buffer_t buffer,
                       byte_t value)
{
    assert(fa_atomic_ring_buffer_can_write(buffer, 1) && "Overflow");
    {
        buffer->data[buffer->last] = value;
        buffer->last = (buffer->last + 1) % buffer->size;
        // buffer->count = buffer->count + 1;

        // DEBUG
        // OSAtomicAdd32Barrier((int32_t) 1, (int32_t*) &buffer->count);
        fa_atomic_add(buffer->count, 1);
    }
    return true;
}

// TODO more efficient versions
// Can using memcpy + slicing or virtual memory

size_t fa_atomic_ring_buffer_read_many(byte_t *dst,
                                       ring_buffer_t src,
                                       size_t count)
{
    if (fa_atomic_ring_buffer_can_read(src, count)) {
        for (size_t i = 0; i < count; ++i) {
            dst[i] = unsafe_read_byte(src);
        }

        return count;
    } else {
        if (src->status == buffer_alright) {
            src->status = buffer_underflowed;
            char msg[100];
            sprintf(msg, "Underflow: count=%zu, size=%zu\n", fa_atomic_ring_buffer_remaining(src), fa_atomic_ring_buffer_size(src));
            warn(string(msg));
        }
        return 0;
    }
}

size_t fa_atomic_ring_buffer_write_many(ring_buffer_t dst,
                                        byte_t *src,
                                        size_t count)
{
    if (fa_atomic_ring_buffer_can_write(dst, count)) {
        for (size_t i = 0; i < count; ++i) {
            unsafe_write_byte(dst, src[i]);
        }
        return count;
    } else {
        if (dst->status == buffer_alright) {
            dst->status = buffer_overflowed;
            char msg[100];
            sprintf(msg, "Overflow: count=%zu, size=%zu\n", fa_atomic_ring_buffer_remaining(dst), fa_atomic_ring_buffer_size(dst));
            warn(string(msg));
        }
        return 0;
    }
}


bool fa_atomic_ring_buffer_read(fa_atomic_ring_buffer_t buffer, byte_t *value)
{
    return fa_atomic_ring_buffer_read_many((byte_t *) value, buffer, sizeof(byte_t)) > 0;
}

bool fa_atomic_ring_buffer_read_float(fa_atomic_ring_buffer_t buffer, float *value)
{
    return fa_atomic_ring_buffer_read_many((byte_t *) value, buffer, sizeof(float)) > 0;
}

bool fa_atomic_ring_buffer_read_double(fa_atomic_ring_buffer_t buffer, double *value)
{
    return fa_atomic_ring_buffer_read_many((byte_t *) value, buffer, sizeof(double)) > 0;
}

bool fa_atomic_ring_buffer_write(ring_buffer_t buffer, byte_t value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(byte_t)) > 0;
}

bool fa_atomic_ring_buffer_write_float(ring_buffer_t buffer, float value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(float)) > 0;
}

bool fa_atomic_ring_buffer_write_double(ring_buffer_t buffer, double value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(double)) > 0;
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

