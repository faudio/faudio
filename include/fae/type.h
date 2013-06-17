
#ifndef _FAE_TYPE
#define _FAE_TYPE

#include <fae.h>
#include <fae/pair.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeType Type
    @{
    */

typedef size_t fae_type_frames_t;
typedef enum {
            unit_type,
            i8_type,
            i16_type,
            i32_type,
            i64_type,
            f32_type,
            f64_type,
            ptr_type
        } fae_type_simple_t;
typedef struct _fae_type_t * fae_type_t;
fae_type_t fae_type_simple(fae_type_simple_t);
fae_type_t fae_type_pair(fae_type_t, fae_type_t);
fae_type_t fae_type_vector(fae_type_t, size_t);
fae_type_t fae_type_frame(fae_type_t);
fae_type_t fae_type_copy(fae_type_t);
void fae_type_destroy(fae_type_t);
fae_type_t fae_type_repeat(int, fae_type_t);
bool fae_type_is_simple(fae_type_t);
bool fae_type_is_pair(fae_type_t);
bool fae_type_is_vector(fae_type_t);
bool fae_type_is_frame(fae_type_t);
int fae_type_channels(fae_type_t);
fae_type_simple_t fae_type_get_simple(fae_type_t);
fae_type_t fae_type_get_pair_fst(fae_type_t);
fae_type_t fae_type_get_pair_snd(fae_type_t);
fae_type_t fae_type_get_vector_base(fae_type_t);
size_t fae_type_get_vector_size(fae_type_t);
fae_type_t fae_type_get_frame_base(fae_type_t);

/** @}
    @}
    */

#endif // _FAE_TYPE

