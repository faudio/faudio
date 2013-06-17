
#ifndef _FAE_DYNAMIC
#define _FAE_DYNAMIC

#include <fae.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeDynamic Dynamic
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
        } fae_dynamic_type_repr_t;
typedef struct {
            fae_dynamic_type_repr_t (* get_type)(fae_ptr_t);
        } fae_dynamic_t;
bool fae_dynamic_check(fae_ptr_t);
fae_dynamic_type_repr_t fae_dynamic_get_type(fae_ptr_t);

/** @}
    @}
    */

#endif // _FAE_DYNAMIC

