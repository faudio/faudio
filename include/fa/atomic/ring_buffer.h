
#ifndef _FA_ATOMIC_RINGBUFFER
#define _FA_ATOMIC_RINGBUFFER

#include <fa.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    @defgroup FaAtomicRingBuffer RingBuffer
    @{
    */

typedef struct _fa_atomic_ring_buffer_t * fa_atomic_ring_buffer_t;
fa_atomic_ring_buffer_t fa_atomic_ring_buffer_create(size_t);
fa_atomic_ring_buffer_t fa_atomic_ring_buffer_copy(fa_atomic_ring_buffer_t);
fa_atomic_ring_buffer_t fa_atomic_ring_buffer_resize(size_t,
                                                     fa_atomic_ring_buffer_t);
void fa_atomic_ring_buffer_swap(fa_atomic_ring_buffer_t,
                                fa_atomic_ring_buffer_t);
void fa_atomic_ring_buffer_destroy(fa_atomic_ring_buffer_t);
size_t fa_atomic_ring_buffer_size(fa_atomic_ring_buffer_t);
uint8_t fa_atomic_ring_buffer_read(fa_atomic_ring_buffer_t);
bool fa_atomic_ring_buffer_write(fa_atomic_ring_buffer_t, uint8_t);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_RINGBUFFER

