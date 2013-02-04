
#ifndef _DOREMIR_TYPE
#define _DOREMIR_TYPE

#include <doremir.h>
#include <doremir/pair.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirType Type
    @{
    */

typedef size_t doremir_type_frames_t;
typedef enum {
            unit_type,
            i8_type,
            i16_type,
            i32_type,
            i64_type,
            f32_type,
            f64_type,
            ptr_type
        } doremir_type_simple_t;
typedef struct _doremir_type_t * doremir_type_t;
doremir_type_t doremir_type_simple(doremir_type_simple_t);
doremir_type_t doremir_type_pair(doremir_type_t, doremir_type_t);
doremir_type_t doremir_type_vector(doremir_type_t, size_t);
doremir_type_t doremir_type_frame(doremir_type_t);
doremir_type_t doremir_type_copy(doremir_type_t);
void doremir_type_destroy(doremir_type_t);
doremir_type_t doremir_type_repeat(int, doremir_type_t);
int doremir_type_channels(doremir_type_t);
bool doremir_type_is_simple(doremir_type_t);
bool doremir_type_is_pair(doremir_type_t);
bool doremir_type_is_vector(doremir_type_t);
bool doremir_type_is_frame(doremir_type_t);
doremir_type_simple_t doremir_type_get_simple(doremir_type_t);
doremir_type_t doremir_type_get_pair_fst(doremir_type_t);
doremir_type_t doremir_type_get_pair_snd(doremir_type_t);
doremir_type_t doremir_type_get_vector_base(doremir_type_t);
size_t doremir_type_get_vector_size(doremir_type_t);
doremir_type_t doremir_type_get_frame_base(doremir_type_t);
size_t doremir_type_size_of(doremir_type_frames_t, doremir_type_t);
size_t doremir_type_offset_of(doremir_type_frames_t,
                              doremir_type_t);
size_t doremir_type_align_of(doremir_type_t);

/** @}
    @}
    */

#endif // _DOREMIR_TYPE

