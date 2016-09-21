
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/atomic/ring_buffer.h>
#include <fa/atomic.h>
#include <fa/buffer.h>
#include <fa/string.h>
#include <fa/dynamic.h>
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
typedef fa_atomic_ring_buffer_t ring_fa_buffer_t;

struct _fa_atomic_ring_buffer_t {

    fa_impl_t           impl;                   //  Interface dispatcher

    size_t              size;                   //  Size (immutable)
    size_t              first, last;            //  Next read or write, always < size
    fa_atomic_t         count;                  //  Bytes written not yet read, always <= size

    //  if count == size, the buffer is full
    //  if count == 0,    the buffer is empty
    //  We can always read n bytes, where n == count
    //  We can always write n bytes, where n == (size-count)

    byte_t              *data;                  //  Memory region (data..data+size) [0,1,2,3,4]

    fa_atomic_t         closed;

    fa_atomic_t         ref_count;
    fa_atomic_t         marked_for_destruction;
    
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
ring_fa_buffer_t fa_atomic_ring_buffer_create(size_t size)
{
    fa_ptr_t atomic_ring_buffer_impl(fa_id_t interface);

    fa_atomic_ring_buffer_t b = fa_new(atomic_ring_buffer);
    b->impl     = &atomic_ring_buffer_impl;

    b->data     = fa_malloc(size/*+1*/);
    b->size     = size;

    b->first    = 0;
    b->last     = 0;
    b->count    = fa_atomic();
    
    b->status   = buffer_alright;
    b->closed   = fa_atomic();
    fa_atomic_set(b->closed, (fa_ptr_t)false);
    
    b->ref_count              = fa_atomic();
    b->marked_for_destruction = fa_atomic();
    fa_atomic_set(b->ref_count, (fa_ptr_t)0);
    fa_atomic_set(b->marked_for_destruction, (fa_ptr_t)false);

    return b;
}

#define atomic_get_size(A) ((size_t) fa_atomic_get(A))

static inline void do_destroy_ring_buffer(ring_fa_buffer_t buffer)
{
    //fa_slog_info("do_destroy_ring_buffer");
    fa_destroy(buffer->count);
    fa_destroy(buffer->closed);
    fa_destroy(buffer->ref_count);
    fa_destroy(buffer->marked_for_destruction);
    fa_free(buffer->data);
    fa_delete(buffer);
}


void fa_atomic_ring_buffer_destroy(ring_fa_buffer_t buffer)
{
    //fa_slog_info("fa_atomic_ring_buffer_destroy");
    if ((size_t)fa_atomic_get(buffer->ref_count) == 0) {
        do_destroy_ring_buffer(buffer);
    } else {
        fa_atomic_set(buffer->marked_for_destruction, (fa_ptr_t) true);
    }
}

size_t fa_atomic_ring_buffer_size(ring_fa_buffer_t buffer)
{
    return buffer->size;
}

size_t fa_atomic_ring_buffer_remaining(ring_fa_buffer_t buffer)
{
    return atomic_get_size(buffer->count);
}

double fa_atomic_ring_buffer_filled(ring_fa_buffer_t buffer)
{
    size_t rem  = fa_atomic_ring_buffer_remaining(buffer);
    size_t size = fa_atomic_ring_buffer_size(buffer);
    return (double) rem / (double) size;
}


bool fa_atomic_ring_buffer_can_read(ring_fa_buffer_t buffer, size_t n)
{
    return atomic_get_size(buffer->count) >= n;
}

bool fa_atomic_ring_buffer_can_write(ring_fa_buffer_t buffer, size_t n)
{
    return (atomic_get_size(buffer->count) + n) <= buffer->size;
}

void fa_atomic_ring_buffer_close(ring_fa_buffer_t buffer)
{
    fa_atomic_set(buffer->closed, (fa_ptr_t)true);
}

void fa_atomic_ring_buffer_reset(ring_fa_buffer_t buffer)
{
    // Note: does not reset reference counting
    buffer->first    = 0;
    buffer->last     = 0;
    fa_atomic_set(buffer->count, (fa_ptr_t)0);
    fa_atomic_set(buffer->closed, (fa_ptr_t)false);
    buffer->status   = buffer_alright;
}

bool fa_atomic_ring_buffer_is_closed(ring_fa_buffer_t buffer)
{
    return (bool)fa_atomic_get(buffer->closed);
}

void fa_atomic_ring_buffer_take_reference(ring_fa_buffer_t buffer)
{
    fa_atomic_add(buffer->ref_count, 1);
}

void fa_atomic_ring_buffer_release_reference(ring_fa_buffer_t buffer)
{
    fa_atomic_add(buffer->ref_count, -1);
    // TODO: is a lock needed?
    if ((size_t)fa_atomic_get(buffer->ref_count) == 0 && (bool)fa_atomic_get(buffer->marked_for_destruction)) {
        do_destroy_ring_buffer(buffer);
    }
}

byte_t unsafe_read_byte(ring_fa_buffer_t buffer)
{
    byte_t x;
    assert(fa_atomic_ring_buffer_can_read(buffer, 1) && "Underflow");
    {
        x = buffer->data[buffer->first];
        buffer->first = (buffer->first + 1) % buffer->size;
        fa_atomic_add(buffer->count, -1);
    }
    return x;
}

bool unsafe_write_byte(ring_fa_buffer_t buffer,
                       byte_t value)
{
    assert(fa_atomic_ring_buffer_can_write(buffer, 1) && "Overflow");
    {
        buffer->data[buffer->last] = value;
        buffer->last = (buffer->last + 1) % buffer->size;
        fa_atomic_add(buffer->count, 1);
    }
    return true;
}


/*
    TODO More efficient versions
    Can using memcpy + slicing or virtual memory
*/

size_t fa_atomic_ring_buffer_read_many(byte_t *dst,
                                       ring_fa_buffer_t src,
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
            sprintf(msg,
                    "Underflow: count=%zu, size=%zu\n",
                    fa_atomic_ring_buffer_remaining(src),
                    fa_atomic_ring_buffer_size(src)
                   );
            fa_warn(fa_string_from_utf8(msg));
        }

        return 0;
    }
}

size_t fa_atomic_ring_buffer_write_many(ring_fa_buffer_t dst,
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
            sprintf(msg,
                    "Overflow: count=%zu, size=%zu\n",
                    fa_atomic_ring_buffer_remaining(dst),
                    fa_atomic_ring_buffer_size(dst)
                   );
            fa_warn(fa_string_from_utf8(msg));
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

bool fa_atomic_ring_buffer_read_long(fa_atomic_ring_buffer_t buffer, long *value)
{
    return fa_atomic_ring_buffer_read_many((byte_t *) value, buffer, sizeof(long)) > 0;
}

bool fa_atomic_ring_buffer_write(ring_fa_buffer_t buffer, byte_t value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(byte_t)) > 0;
}

bool fa_atomic_ring_buffer_write_float(ring_fa_buffer_t buffer, float value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(float)) > 0;
}

bool fa_atomic_ring_buffer_write_double(ring_fa_buffer_t buffer, double value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(double)) > 0;
}

bool fa_atomic_ring_buffer_write_long(ring_fa_buffer_t buffer, long value)
{
    return fa_atomic_ring_buffer_write_many(buffer, (byte_t *) &value, sizeof(long)) > 0;
}



// --------------------------------------------------------------------------------

fa_string_t atomic_ring_buffer_show(fa_ptr_t v)
{
    fa_string_t s = fa_string("<RingBuffer ");
    s = fa_string_dappend(s, fa_format_integral("%p", (long) v));
    s = fa_string_dappend(s, fa_string(">"));
    return s;
}

void atomic_ring_buffer_destroy(fa_ptr_t a)
{
    fa_atomic_ring_buffer_destroy(a);
}

void atomic_ring_buffer_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    if (p(a)) fa_atomic_ring_buffer_destroy(a);
}

fa_dynamic_type_repr_t atomic_ring_buffer_get_type(fa_ptr_t a) {
    return atomic_ring_buffer_type_repr;
}

fa_ptr_t atomic_ring_buffer_impl(fa_id_t interface)
{
    static fa_string_show_t atomic_ring_buffer_show_impl = { atomic_ring_buffer_show };
    static fa_destroy_t atomic_ring_buffer_destroy_impl = { atomic_ring_buffer_destroy, atomic_ring_buffer_deep_destroy };
    static fa_dynamic_t atomic_ring_buffer_dynamic_impl = { atomic_ring_buffer_get_type };

    switch (interface) {
    case fa_string_show_i:
        return &atomic_ring_buffer_show_impl;

    case fa_destroy_i:
        return &atomic_ring_buffer_destroy_impl;
        
    case fa_dynamic_i:
        return &atomic_ring_buffer_dynamic_impl;

    default:
        return NULL;
    }
}

