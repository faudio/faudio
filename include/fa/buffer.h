
#ifndef _FA_BUFFER
#define _FA_BUFFER

#include <fa.h>
#include <fa/std.h>
#include <fa/pair.h>
#include <fa/string.h>
#include <fa/type.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaBuffer Buffer
    @{
    */

typedef struct _fa_buffer_t * fa_buffer_t;
fa_buffer_t fa_buffer_create(size_t);
fa_buffer_t fa_buffer_wrap(fa_ptr_t, size_t, fa_unary_t, fa_ptr_t);
fa_buffer_t fa_buffer_copy(fa_buffer_t);
fa_buffer_t fa_buffer_resize(size_t, fa_buffer_t);
void fa_buffer_destroy(fa_buffer_t);
size_t fa_buffer_size(fa_buffer_t);
uint8_t fa_buffer_get(fa_buffer_t, size_t);
void fa_buffer_set(fa_buffer_t, size_t, uint8_t);
int16_t fa_buffer_get_int16(fa_buffer_t, size_t);
int32_t fa_buffer_get_int32(fa_buffer_t, size_t);
int64_t fa_buffer_get_int64(fa_buffer_t, size_t);
float fa_buffer_get_float(fa_buffer_t, size_t);
double fa_buffer_get_double(fa_buffer_t, size_t);
void fa_buffer_set_int16(fa_buffer_t, size_t, int16_t);
void fa_buffer_set_int32(fa_buffer_t, size_t, int32_t);
void fa_buffer_set_int64(fa_buffer_t, size_t, int64_t);
void fa_buffer_set_float(fa_buffer_t, size_t, float);
void fa_buffer_set_double(fa_buffer_t, size_t, double);
fa_buffer_t fa_buffer_read_raw(fa_string_file_path_t);
void fa_buffer_write_raw(fa_string_file_path_t, fa_buffer_t);
fa_pair_t fa_buffer_read_audio(fa_string_file_path_t);
void fa_buffer_write_audio(fa_string_file_path_t,
                           fa_type_t,
                           fa_buffer_t);
void * fa_buffer_unsafe_address(fa_buffer_t);

/** @}
    @}
    */

#endif // _FA_BUFFER

