
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


fa_atomic_ring_buffer_t fa_atomic_ring_buffer_create(size_t);


void fa_atomic_ring_buffer_destroy(fa_atomic_ring_buffer_t);


size_t fa_atomic_ring_buffer_size(fa_atomic_ring_buffer_t);


bool fa_atomic_ring_buffer_read(fa_atomic_ring_buffer_t, uint8_t *);


bool fa_atomic_ring_buffer_read_float(fa_atomic_ring_buffer_t,
                                      float *);


bool fa_atomic_ring_buffer_read_double(fa_atomic_ring_buffer_t,
                                       double *);


bool fa_atomic_ring_buffer_write(fa_atomic_ring_buffer_t, uint8_t);


bool fa_atomic_ring_buffer_write_float(fa_atomic_ring_buffer_t,
                                       float);


bool fa_atomic_ring_buffer_write_double(fa_atomic_ring_buffer_t,
                                        double);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_RINGBUFFER

