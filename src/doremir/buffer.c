
#include <doremir/buffer.h>

struct _doremir_buffer_t
{
    size_t size;
    uint8_t * data;
};

/**
    @typedef doremir_buffer_t
    The buffer.
 */

/** Create a new buffer.
    @note
        O(n)
 */
doremir_buffer_t 
doremir_buffer_create(size_t size)
{
    assert(false && "Not implemented");
}

/** Copy the given buffer.
    @note
        O(1)
 */
doremir_buffer_t 
doremir_buffer_copy(doremir_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Swap the contents of the given buffers.
    @note
        O(1)
 */
void 
doremir_buffer_swap(doremir_buffer_t a, doremir_buffer_t b)
{
    assert(false && "Not implemented");
}

/** Destroy the given buffer.
    @note
        O(1)
 */
void 
doremir_buffer_destroy(doremir_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Return the size of the buffer.
    @note
        O(n)
 */
size_t 
doremir_buffer_size(doremir_buffer_t buffer)
{
    assert(false && "Not implemented");
}

/** Read a value from the buffer.
    @note
        O(1)
 */
uint8_t 
doremir_buffer_peek(doremir_buffer_t buffer, int index)
{
    assert(false && "Not implemented");
}

/** Update a value in the buffer.
    @note
        O(1)
 */
void 
doremir_buffer_poke(doremir_buffer_t buffer, int index, uint8_t value)
{
    assert(false && "Not implemented");
}

