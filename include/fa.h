
#ifndef _FA
#define _FA

#include <fa/std.h>
#include <fa/alloc.h>
#include <fa/interfaces.h>

/** @defgroup Fa Fa
    @{
    */

typedef void * fa_ptr_t;
typedef fa_ptr_t (* fa_nullary_t)(fa_ptr_t);
typedef fa_ptr_t (* fa_unary_t)(fa_ptr_t, fa_ptr_t);
typedef fa_ptr_t (* fa_binary_t)(fa_ptr_t, fa_ptr_t, fa_ptr_t);
typedef fa_ptr_t (* fa_ternary_t)(fa_ptr_t,
                                  fa_ptr_t,
                                  fa_ptr_t,
                                  fa_ptr_t);
typedef bool (* fa_pred_t)(fa_ptr_t, fa_ptr_t);
typedef char fa_char8_t;
typedef uint16_t fa_char16_t;
typedef uint32_t fa_char32_t;
bool fa_is_bool(fa_ptr_t);
bool fa_is_int8(fa_ptr_t);
bool fa_is_int16(fa_ptr_t);
bool fa_is_int32(fa_ptr_t);
bool fa_is_int64(fa_ptr_t);
bool fa_is_float(fa_ptr_t);
bool fa_is_double(fa_ptr_t);
bool fa_is_ref(fa_ptr_t);
bool fa_to_bool(fa_ptr_t);
int8_t fa_to_int8(fa_ptr_t);
int16_t fa_to_int16(fa_ptr_t);
int32_t fa_to_int32(fa_ptr_t);
int64_t fa_to_int64(fa_ptr_t);
float fa_to_float(fa_ptr_t);
double fa_to_double(fa_ptr_t);
bool fa_peek_bool(fa_ptr_t);
int8_t fa_peek_int8(fa_ptr_t);
int16_t fa_peek_int16(fa_ptr_t);
int32_t fa_peek_int32(fa_ptr_t);
int64_t fa_peek_int64(fa_ptr_t);
float fa_peek_float(fa_ptr_t);
double fa_peek_double(fa_ptr_t);
fa_ptr_t fa_from_bool(bool);
fa_ptr_t fa_from_int8(int8_t);
fa_ptr_t fa_from_int16(int16_t);
fa_ptr_t fa_from_int32(int32_t);
fa_ptr_t fa_from_int64(int64_t);
fa_ptr_t fa_from_float(float);
fa_ptr_t fa_from_double(double);
typedef int64_t fa_id_t;
typedef fa_ptr_t (* fa_impl_t)(fa_id_t);
fa_ptr_t fa_interface(fa_id_t, fa_ptr_t);
typedef struct {
            bool (* equal)(fa_ptr_t, fa_ptr_t);
        } fa_equal_t;
bool fa_equal(fa_ptr_t, fa_ptr_t);
bool fa_not_equal(fa_ptr_t, fa_ptr_t);
typedef struct {
            bool (* less_than)(fa_ptr_t, fa_ptr_t);
            bool (* greater_than)(fa_ptr_t, fa_ptr_t);
        } fa_order_t;
bool fa_less_than(fa_ptr_t, fa_ptr_t);
bool fa_greater_than(fa_ptr_t, fa_ptr_t);
bool fa_less_than_equal(fa_ptr_t, fa_ptr_t);
bool fa_greater_than_equal(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_min(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_max(fa_ptr_t, fa_ptr_t);
typedef struct {
            fa_ptr_t (* add)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* subtract)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* multiply)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* divide)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* absolute)(fa_ptr_t);
        } fa_number_t;
fa_ptr_t fa_add(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_subtract(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_multiply(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_divide(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_absolute(fa_ptr_t);
fa_ptr_t fa_dadd(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_dsubtract(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_dmultiply(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_ddivide(fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_dabsolute(fa_ptr_t);
typedef struct {
            fa_ptr_t (* copy)(fa_ptr_t);
        } fa_copy_t;
typedef struct {
            void (* destroy)(fa_ptr_t);
        } fa_destroy_t;
fa_ptr_t fa_copy(fa_ptr_t);
fa_ptr_t fa_move(fa_ptr_t);
void fa_destroy(fa_ptr_t);
bool fa_check(fa_ptr_t);
void fa_print(char *, fa_ptr_t);
void fa_dprint(char *, fa_ptr_t);
void fa_print_ln(fa_ptr_t);
void fa_dprint_ln(fa_ptr_t);

/** @}
    */

#endif // _FA

