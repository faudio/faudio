
#ifndef _DOREMIR_BUFFER
#define _DOREMIR_BUFFER

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirBuffer Buffer
    @{
    */

typedef struct _doremir_buffer_t * doremir_buffer_t;
doremir_buffer_t doremir_buffer_create(size_t);
doremir_buffer_t doremir_buffer_copy(doremir_buffer_t);
doremir_buffer_t doremir_buffer_copy_sized(size_t,
                                           doremir_buffer_t);
void doremir_buffer_swap(doremir_buffer_t, doremir_buffer_t);
void doremir_buffer_destroy(doremir_buffer_t);
size_t doremir_buffer_size(doremir_buffer_t);
uint8_t doremir_buffer_peek(doremir_buffer_t, int);
void doremir_buffer_poke(doremir_buffer_t, int, uint8_t);

/** @}
    @}
    */

#endif // _DOREMIR_BUFFER

