
#ifndef _FA_TYPE
#define _FA_TYPE

#include <fa.h>
#include <fa/pair.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaType Type
    @{
    */

typedef size_t fa_type_frames_t;
typedef enum {
            unit_type,
            i8_type,
            i16_type,
            i32_type,
            i64_type,
            f32_type,
            f64_type,
            ptr_type
        } fa_type_simple_t;
typedef struct _fa_type_t * fa_type_t;
fa_type_t fa_type_simple(fa_type_simple_t);
fa_type_t fa_type_pair(fa_type_t, fa_type_t);
fa_type_t fa_type_vector(fa_type_t, size_t);
fa_type_t fa_type_frame(fa_type_t);
fa_type_t fa_type_copy(fa_type_t);
void fa_type_destroy(fa_type_t);
fa_type_t fa_type_repeat(int, fa_type_t);
bool fa_type_is_simple(fa_type_t);
bool fa_type_is_pair(fa_type_t);
bool fa_type_is_vector(fa_type_t);
bool fa_type_is_frame(fa_type_t);
int fa_type_channels(fa_type_t);
fa_type_simple_t fa_type_get_simple(fa_type_t);
fa_type_t fa_type_get_pair_fst(fa_type_t);
fa_type_t fa_type_get_pair_snd(fa_type_t);
fa_type_t fa_type_get_vector_base(fa_type_t);
size_t fa_type_get_vector_size(fa_type_t);
fa_type_t fa_type_get_frame_base(fa_type_t);

/** @}
    @}
    */

#endif // _FA_TYPE

