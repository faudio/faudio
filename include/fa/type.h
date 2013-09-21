
#ifndef _FA_TYPE
#define _FA_TYPE

#include <fa.h>
#include <fa/pair.h>

/** @addtogroup FaType

    @addtogroup FaType
    
    Provides audio types. An *audio type* is either a simple type such as `i8`, or a
    compound such as `(i8, i8)`. Compound types are constructed from simple types.
    
    Each signals has an associated type, which is checked at runtime.

    Type            | Description
    ----------------| ----------------------------
    `i8`            | 8-bit integer
    `i16`           | 16-bit integer
    `i32`           | 32-bit integer
    `i64`           | 64-bit integer
    `f32`           | 32-bit floating point
    `f64`           | 64-bit floating point
    `()`            | Unit type
    `(A, B)`        | Pair of *A* and *B*
    `[A x N]`       | Vector of *A*
    `{A}`           | Frame of *A*

    @par Literals
    - `type(unit)`
    - `type(i8)`
    - `type(i16)`
    - `type(i32)`
    - `type(i64)`
    - `type(f32)`
    - `type(f64)`
    - `type(ptr)`
    - `type_pair(type(f64), type(f64))`
    - `type_vector(type(f64), 200)`
    - `type_frame(type(f64))`

    @par Implements        
    - fa_equal_t
    - fa_copy_t
    - fa_string_show_t
    - fa_destroy_t
    
 
    @defgroup Fa Fa
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

