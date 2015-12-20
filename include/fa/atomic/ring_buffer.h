
#ifndef _FA_ATOMIC_RINGBUFFER
#define _FA_ATOMIC_RINGBUFFER

#include <fa.h>

/** @addtogroup FaAtomicRingBuffer
 
    A bounded lock-free queue of raw data.
    
    Each ring buffer single-ownership semantics. The `close` method does not destroy the ring buffer,
    but provides a way for the writer to notify the reader of the end of the sequence.
    
    The *size* of a buffer can never change.
    
    @par Literals
    - `atomic_ring_buffer(size)` 

    @par Implements 
    - fa_destroy_t
    - fa_string_show_t

    @see 
    - [Data structures](@ref DataStructures)

 
    @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    @defgroup FaAtomicRingBuffer RingBuffer
    @{
    */


typedef struct _fa_atomic_ring_buffer_t * fa_atomic_ring_buffer_t;

/** Create a ring buffer of the given size.
*/
fa_atomic_ring_buffer_t fa_atomic_ring_buffer_create(size_t size_);

/** Destroy the given ring buffer. If the reference count is greater than zero,
    the actual destruction is postponed until the reference count reaches
    zero. See @ref fa_buffer_take_reference and @ref fa_buffer_release_reference.
    
    @note
        O(n)
*/
void fa_atomic_ring_buffer_destroy(fa_atomic_ring_buffer_t ringBuffer);

/** Destroy the given ring buffer.
*/
size_t fa_atomic_ring_buffer_size(fa_atomic_ring_buffer_t ringBuffer);

/** Return `used/size` as a fraction in the range `0-1`.
    
*/
double fa_atomic_ring_buffer_filled(fa_atomic_ring_buffer_t ringBuffer);

/** Notify downstream sinks of end of data.
    Subsequent calls to `write` will fail.
*/
void fa_atomic_ring_buffer_close(fa_atomic_ring_buffer_t ringBuffer);

/** Empty the ring buffer and its reading pointer.
    NOTE: reference counts are not reset!
    NOTE: Not atomic!
*/
void fa_atomic_ring_buffer_reset(fa_atomic_ring_buffer_t ringBuffer);

/** Whether the buffer have been closed by an upstream source.
*/
bool fa_atomic_ring_buffer_is_closed(fa_atomic_ring_buffer_t ringBuffer);

/** Take a reference to the ring buffer, i.e. increase its reference count.
    Atomic.
*/
void fa_atomic_ring_buffer_take_reference(fa_atomic_ring_buffer_t buffer);

/** Release a reference to the ring buffer, i.e. decrease its reference count.
    If the reference count reaches 0 and destroy has previously been
    called on the buffer, the buffer will be destroyed.
    Atomic.
*/
void fa_atomic_ring_buffer_release_reference(fa_atomic_ring_buffer_t buffer);


/** If true, a subsequent call to `read` with a type of the given
    size is guaranteed to succeed.
*/
bool fa_atomic_ring_buffer_can_read(fa_atomic_ring_buffer_t ringBuffer,
                                    size_t size_);

/** If true, a subsequent call to `write` with a type of the given
    size is guaranteed to succeed.
*/
bool fa_atomic_ring_buffer_can_write(fa_atomic_ring_buffer_t ringBuffer,
                                     size_t size_);

/** Read a single byte value.
*/
bool fa_atomic_ring_buffer_read(fa_atomic_ring_buffer_t ringBuffer,
                                uint8_t *);

/** Read a single float value.
*/
bool fa_atomic_ring_buffer_read_float(fa_atomic_ring_buffer_t ringBuffer,
                                      float *);

/** Read a single double value.
*/
bool fa_atomic_ring_buffer_read_double(fa_atomic_ring_buffer_t ringBuffer,
                                       double *);
                                       
/** Read a single long value.
*/
bool fa_atomic_ring_buffer_read_long(fa_atomic_ring_buffer_t ringBuffer,
                                     long *);

/** Write a single byte value.
*/
bool fa_atomic_ring_buffer_write(fa_atomic_ring_buffer_t ringBuffer,
                                 uint8_t uInt8_);

/** Write a single float value.
*/
bool fa_atomic_ring_buffer_write_float(fa_atomic_ring_buffer_t ringBuffer,
                                       float float_);

/** Write a single double value.
*/
bool fa_atomic_ring_buffer_write_double(fa_atomic_ring_buffer_t ringBuffer,
                                        double double_);
                                        
/** Write a single long value.
*/
bool fa_atomic_ring_buffer_write_long(fa_atomic_ring_buffer_t ringBuffer,
                                      long long_);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_RINGBUFFER

