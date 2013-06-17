
#ifndef _FAE_ATOMIC_RINGBUFFER
#define _FAE_ATOMIC_RINGBUFFER

#include <fae.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeAtomic Atomic
    @{
    @defgroup FaeAtomicRingBuffer RingBuffer
    @{
    */

typedef struct _fae_atomic_ring_buffer_t * fae_atomic_ring_buffer_t;
fae_atomic_ring_buffer_t fae_atomic_ring_buffer_create(size_t);
fae_atomic_ring_buffer_t fae_atomic_ring_buffer_copy(fae_atomic_ring_buffer_t);
fae_atomic_ring_buffer_t fae_atomic_ring_buffer_resize(size_t,
                                                       fae_atomic_ring_buffer_t);
void fae_atomic_ring_buffer_swap(fae_atomic_ring_buffer_t,
                                 fae_atomic_ring_buffer_t);
void fae_atomic_ring_buffer_destroy(fae_atomic_ring_buffer_t);
size_t fae_atomic_ring_buffer_size(fae_atomic_ring_buffer_t);
uint8_t fae_atomic_ring_buffer_read(fae_atomic_ring_buffer_t);
bool fae_atomic_ring_buffer_write(fae_atomic_ring_buffer_t,
                                  uint8_t);

/** @}
    @}
    @}
    */

#endif // _FAE_ATOMIC_RINGBUFFER

