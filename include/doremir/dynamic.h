
#ifndef _DOREMIR_DYNAMIC
#define _DOREMIR_DYNAMIC

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDynamic Dynamic
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
            string_type_repr
        } doremir_dynamic_type_repr_t;
typedef struct {
            doremir_dynamic_type_repr_t (* get_type)(doremir_ptr_t);
        } doremir_dynamic_t;
doremir_dynamic_type_repr_t doremir_dynamic_get_type(doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_DYNAMIC

