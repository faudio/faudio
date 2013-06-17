
#ifndef _FAE_BUFFER
#define _FAE_BUFFER

#include <fae.h>
#include <fae/std.h>
#include <fae/pair.h>
#include <fae/string.h>
#include <fae/type.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeBuffer Buffer
    @{
    */

typedef struct _fae_buffer_t * fae_buffer_t;
fae_buffer_t fae_buffer_create(size_t);
fae_buffer_t fae_buffer_copy(fae_buffer_t);
fae_buffer_t fae_buffer_resize(size_t, fae_buffer_t);
void fae_buffer_destroy(fae_buffer_t);
size_t fae_buffer_size(fae_buffer_t);
uint8_t fae_buffer_get(fae_buffer_t, size_t);
void fae_buffer_set(fae_buffer_t, size_t, uint8_t);
int16_t fae_buffer_get_int16(fae_buffer_t, size_t);
int32_t fae_buffer_get_int32(fae_buffer_t, size_t);
int64_t fae_buffer_get_int64(fae_buffer_t, size_t);
float fae_buffer_get_float(fae_buffer_t, size_t);
double fae_buffer_get_double(fae_buffer_t, size_t);
void fae_buffer_set_int16(fae_buffer_t, size_t, int16_t);
void fae_buffer_set_int32(fae_buffer_t, size_t, int32_t);
void fae_buffer_set_int64(fae_buffer_t, size_t, int64_t);
void fae_buffer_set_float(fae_buffer_t, size_t, float);
void fae_buffer_set_double(fae_buffer_t, size_t, double);
fae_buffer_t fae_buffer_read_raw(fae_string_file_path_t);
void fae_buffer_write_raw(fae_string_file_path_t, fae_buffer_t);
fae_pair_t fae_buffer_read_audio(fae_string_file_path_t);
void fae_buffer_write_audio(fae_string_file_path_t,
                            fae_type_t,
                            fae_buffer_t);
void * fae_buffer_unsafe_address(fae_buffer_t);

/** @}
    @}
    */

#endif // _FAE_BUFFER

