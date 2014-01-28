
#ifndef _FA_ATOMIC_RINGBUFFER
#define _FA_ATOMIC_RINGBUFFER

#include <fa.h>

/** @addtogroup FaAtomicRingBuffer
 
    A byte-level, bounded lock-free queue.
    
    This structure is called a buffer for historical reasons, it is more accurately
    a queue and does not support random accces.
    
    @warning
        Not fully implemented.
        

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


fa_atomic_ring_buffer_t fa_atomic_ring_buffer_create(size_t size_);


void fa_atomic_ring_buffer_destroy(fa_atomic_ring_buffer_t ringBuffer);


size_t fa_atomic_ring_buffer_size(fa_atomic_ring_buffer_t ringBuffer);


void fa_atomic_ring_buffer_close(fa_atomic_ring_buffer_t ringBuffer);


bool fa_atomic_ring_buffer_is_closed(fa_atomic_ring_buffer_t ringBuffer);


bool fa_atomic_ring_buffer_can_read(fa_atomic_ring_buffer_t ringBuffer,
                                    size_t size_);


bool fa_atomic_ring_buffer_can_write(fa_atomic_ring_buffer_t ringBuffer,
                                     size_t size_);


bool fa_atomic_ring_buffer_read(fa_atomic_ring_buffer_t ringBuffer,
                                uint8_t *);


bool fa_atomic_ring_buffer_read_float(fa_atomic_ring_buffer_t ringBuffer,
                                      float *);


bool fa_atomic_ring_buffer_read_double(fa_atomic_ring_buffer_t ringBuffer,
                                       double *);


bool fa_atomic_ring_buffer_write(fa_atomic_ring_buffer_t ringBuffer,
                                 uint8_t uInt8_);


bool fa_atomic_ring_buffer_write_float(fa_atomic_ring_buffer_t ringBuffer,
                                       float float_);


bool fa_atomic_ring_buffer_write_double(fa_atomic_ring_buffer_t ringBuffer,
                                        double double_);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_RINGBUFFER

