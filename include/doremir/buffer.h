
#ifndef _DOREMIR_BUFFER
#define _DOREMIR_BUFFER

#include <doremir.h>
#include <doremir/std.h>
#include <doremir/pair.h>
#include <doremir/string.h>
#include <doremir/type.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirBuffer Buffer
    @{
    */

typedef struct _doremir_buffer_t * doremir_buffer_t;
doremir_buffer_t doremir_buffer_create(size_t);
doremir_buffer_t doremir_buffer_copy(doremir_buffer_t);
doremir_buffer_t doremir_buffer_resize(size_t, doremir_buffer_t);
void doremir_buffer_destroy(doremir_buffer_t);
size_t doremir_buffer_size(doremir_buffer_t);
uint8_t doremir_buffer_peek(doremir_buffer_t, size_t);
void doremir_buffer_poke(doremir_buffer_t, size_t, uint8_t);
double doremir_buffer_peek_double(doremir_buffer_t, size_t);
void doremir_buffer_poke_double(doremir_buffer_t, size_t, double);
void * doremir_buffer_unsafe_address(doremir_buffer_t);
doremir_pair_t doremir_buffer_read_audio(doremir_string_file_path_t);
void doremir_buffer_write_audio(doremir_string_file_path_t,
                                doremir_type_t,
                                doremir_buffer_t);

/** @}
    @}
    */

#endif // _DOREMIR_BUFFER

