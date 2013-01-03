
#ifndef _DOREMIR_ATOMIC_RINGBUFFER
#define _DOREMIR_ATOMIC_RINGBUFFER



/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAtomic Atomic
    @{
    @defgroup DoremirAtomicRingBuffer RingBuffer
    @{
    */

typedef struct _doremir_atomic_ring_buffer_t * doremir_atomic_ring_buffer_t;
doremir_atomic_ring_buffer_t doremir_atomic_ring_buffer_create(size_t);
doremir_atomic_ring_buffer_t doremir_atomic_ring_buffer_copy(doremir_atomic_ring_buffer_t);
doremir_atomic_ring_buffer_t doremir_atomic_ring_buffer_copy_sized(size_t,
                                                                   doremir_atomic_ring_buffer_t);
void doremir_atomic_ring_buffer_swap(doremir_atomic_ring_buffer_t,
                                     doremir_atomic_ring_buffer_t);
void doremir_atomic_ring_buffer_destroy(doremir_atomic_ring_buffer_t);
size_t doremir_atomic_ring_buffer_size(doremir_atomic_ring_buffer_t);
uint8_t doremir_atomic_ring_buffer_read(doremir_atomic_ring_buffer_t);
bool doremir_atomic_ring_buffer_write(doremir_atomic_ring_buffer_t,
                                      uint8_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_ATOMIC_RINGBUFFER

