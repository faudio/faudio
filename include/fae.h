
#ifndef _FAE
#define _FAE

#include <fae/std.h>
#include <fae/alloc.h>
#include <fae/interfaces.h>

/** @defgroup Fae Fae
    @{
    */

typedef void * fae_ptr_t;
typedef fae_ptr_t (* fae_nullary_t)(fae_ptr_t);
typedef fae_ptr_t (* fae_unary_t)(fae_ptr_t, fae_ptr_t);
typedef fae_ptr_t (* fae_binary_t)(fae_ptr_t,
                                   fae_ptr_t,
                                   fae_ptr_t);
typedef fae_ptr_t (* fae_ternary_t)(fae_ptr_t,
                                    fae_ptr_t,
                                    fae_ptr_t,
                                    fae_ptr_t);
typedef bool (* fae_pred_t)(fae_ptr_t, fae_ptr_t);
typedef char fae_char8_t;
typedef uint16_t fae_char16_t;
typedef uint32_t fae_char32_t;
bool fae_is_bool(fae_ptr_t);
bool fae_is_int8(fae_ptr_t);
bool fae_is_int16(fae_ptr_t);
bool fae_is_int32(fae_ptr_t);
bool fae_is_int64(fae_ptr_t);
bool fae_is_float(fae_ptr_t);
bool fae_is_double(fae_ptr_t);
bool fae_is_ref(fae_ptr_t);
bool fae_to_bool(fae_ptr_t);
int8_t fae_to_int8(fae_ptr_t);
int16_t fae_to_int16(fae_ptr_t);
int32_t fae_to_int32(fae_ptr_t);
int64_t fae_to_int64(fae_ptr_t);
float fae_to_float(fae_ptr_t);
double fae_to_double(fae_ptr_t);
bool fae_peek_bool(fae_ptr_t);
int8_t fae_peek_int8(fae_ptr_t);
int16_t fae_peek_int16(fae_ptr_t);
int32_t fae_peek_int32(fae_ptr_t);
int64_t fae_peek_int64(fae_ptr_t);
float fae_peek_float(fae_ptr_t);
double fae_peek_double(fae_ptr_t);
fae_ptr_t fae_from_bool(bool);
fae_ptr_t fae_from_int8(int8_t);
fae_ptr_t fae_from_int16(int16_t);
fae_ptr_t fae_from_int32(int32_t);
fae_ptr_t fae_from_int64(int64_t);
fae_ptr_t fae_from_float(float);
fae_ptr_t fae_from_double(double);
typedef int64_t fae_id_t;
typedef fae_ptr_t (* fae_impl_t)(fae_id_t);
fae_ptr_t fae_interface(fae_id_t, fae_ptr_t);
typedef struct {
            bool (* equal)(fae_ptr_t, fae_ptr_t);
        } fae_equal_t;
bool fae_equal(fae_ptr_t, fae_ptr_t);
bool fae_not_equal(fae_ptr_t, fae_ptr_t);
typedef struct {
            bool (* less_than)(fae_ptr_t, fae_ptr_t);
            bool (* greater_than)(fae_ptr_t, fae_ptr_t);
        } fae_order_t;
bool fae_less_than(fae_ptr_t, fae_ptr_t);
bool fae_greater_than(fae_ptr_t, fae_ptr_t);
bool fae_less_than_equal(fae_ptr_t, fae_ptr_t);
bool fae_greater_than_equal(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_min(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_max(fae_ptr_t, fae_ptr_t);
typedef struct {
            fae_ptr_t (* add)(fae_ptr_t, fae_ptr_t);
            fae_ptr_t (* subtract)(fae_ptr_t, fae_ptr_t);
            fae_ptr_t (* multiply)(fae_ptr_t, fae_ptr_t);
            fae_ptr_t (* divide)(fae_ptr_t, fae_ptr_t);
            fae_ptr_t (* absolute)(fae_ptr_t);
        } fae_number_t;
fae_ptr_t fae_add(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_subtract(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_multiply(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_divide(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_absolute(fae_ptr_t);
fae_ptr_t fae_dadd(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_dsubtract(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_dmultiply(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_ddivide(fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_dabsolute(fae_ptr_t);
typedef struct {
            fae_ptr_t (* copy)(fae_ptr_t);
        } fae_copy_t;
typedef struct {
            void (* destroy)(fae_ptr_t);
        } fae_destroy_t;
fae_ptr_t fae_copy(fae_ptr_t);
fae_ptr_t fae_move(fae_ptr_t);
void fae_destroy(fae_ptr_t);
bool fae_check(fae_ptr_t);
void fae_print(char *, fae_ptr_t);
void fae_dprint(char *, fae_ptr_t);

/** @}
    */

#endif // _FAE

