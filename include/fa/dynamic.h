
#ifndef _FA_DYNAMIC
#define _FA_DYNAMIC

#include <fa.h>
#include <fa/string.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaDynamic Dynamic
    @{
    */

typedef enum {
            bool_type_repr,
            i8_type_repr,
            i16_type_repr,
            i32_type_repr,
            i64_type_repr,
            f32_type_repr,
            f64_type_repr,
            pair_type_repr,
            list_type_repr,
            set_type_repr,
            map_type_repr,
            string_type_repr,
            ratio_type_repr
        } fa_dynamic_type_repr_t;
typedef struct {
            fa_dynamic_type_repr_t (* get_type)(fa_ptr_t);
        } fa_dynamic_t;
bool fa_dynamic_check(fa_ptr_t);
fa_dynamic_type_repr_t fa_dynamic_get_type(fa_ptr_t);

/** @}
    @}
    */

#endif // _FA_DYNAMIC

