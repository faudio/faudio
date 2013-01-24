
#ifndef _DOREMIR
#define _DOREMIR

#include <doremir/std.h>
#include <doremir/alloc.h>
#include <doremir/interfaces.h>

/** @defgroup Doremir Doremir
    @{
    */

typedef void * doremir_ptr_t;
typedef doremir_ptr_t (* doremir_nullary_t)(doremir_ptr_t);
typedef doremir_ptr_t (* doremir_unary_t)(doremir_ptr_t,
                                          doremir_ptr_t);
typedef doremir_ptr_t (* doremir_binary_t)(doremir_ptr_t,
                                           doremir_ptr_t,
                                           doremir_ptr_t);
typedef doremir_ptr_t (* doremir_ternary_t)(doremir_ptr_t,
                                            doremir_ptr_t,
                                            doremir_ptr_t,
                                            doremir_ptr_t);
typedef bool (* doremir_pred_t)(doremir_ptr_t, doremir_ptr_t);
typedef char doremir_char8_t;
typedef uint16_t doremir_char16_t;
typedef uint32_t doremir_char32_t;
typedef int64_t doremir_id_t;
typedef doremir_ptr_t (* doremir_impl_t)(doremir_id_t);
doremir_ptr_t doremir_interface(doremir_id_t, doremir_ptr_t);
typedef struct {
            bool (* equal)(doremir_ptr_t, doremir_ptr_t);
        } doremir_equal_t;
typedef struct {
            bool (* less_than)(doremir_ptr_t, doremir_ptr_t);
            bool (* greater_than)(doremir_ptr_t, doremir_ptr_t);
        } doremir_order_t;
typedef struct {
            doremir_ptr_t (* copy)(doremir_ptr_t);
        } doremir_copy_t;
typedef struct {
            void (* destroy)(doremir_ptr_t);
        } doremir_destroy_t;
typedef struct {
            doremir_ptr_t (* add)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* subtract)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* multiply)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* divide)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* absolute)(doremir_ptr_t);
        } doremir_number_t;
bool doremir_equal(doremir_ptr_t, doremir_ptr_t);
bool doremir_not_equal(doremir_ptr_t, doremir_ptr_t);
bool doremir_less_than(doremir_ptr_t, doremir_ptr_t);
bool doremir_greater_than(doremir_ptr_t, doremir_ptr_t);
bool doremir_less_than_equal(doremir_ptr_t, doremir_ptr_t);
bool doremir_greater_than_equal(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_min(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_max(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_add(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_subtract(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_multiply(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_divide(doremir_ptr_t, doremir_ptr_t);
doremir_ptr_t doremir_absolute(doremir_ptr_t);
doremir_ptr_t doremir_copy(doremir_ptr_t);
doremir_ptr_t doremir_move(doremir_ptr_t);
void doremir_destroy(doremir_ptr_t);
void doremir_print(char *, doremir_ptr_t);
void doremir_dprint(char *, doremir_ptr_t);
void doremir_print_ln(doremir_ptr_t);
void doremir_dprint_ln(doremir_ptr_t);
char * doremir_type_str(doremir_ptr_t);
bool doremir_is_bool(doremir_ptr_t);
bool doremir_is_int8(doremir_ptr_t);
bool doremir_is_int16(doremir_ptr_t);
bool doremir_is_int32(doremir_ptr_t);
bool doremir_is_int64(doremir_ptr_t);
bool doremir_is_float(doremir_ptr_t);
bool doremir_is_double(doremir_ptr_t);
bool doremir_is_ref(doremir_ptr_t);
bool doremir_to_bool(doremir_ptr_t);
int8_t doremir_to_int8(doremir_ptr_t);
int16_t doremir_to_int16(doremir_ptr_t);
int32_t doremir_to_int32(doremir_ptr_t);
int64_t doremir_to_int64(doremir_ptr_t);
float doremir_to_float(doremir_ptr_t);
double doremir_to_double(doremir_ptr_t);
bool doremir_peek_bool(doremir_ptr_t);
int8_t doremir_peek_int8(doremir_ptr_t);
int16_t doremir_peek_int16(doremir_ptr_t);
int32_t doremir_peek_int32(doremir_ptr_t);
int64_t doremir_peek_int64(doremir_ptr_t);
float doremir_peek_float(doremir_ptr_t);
double doremir_peek_double(doremir_ptr_t);
doremir_ptr_t doremir_from_bool(bool);
doremir_ptr_t doremir_from_int8(int8_t);
doremir_ptr_t doremir_from_int16(int16_t);
doremir_ptr_t doremir_from_int32(int32_t);
doremir_ptr_t doremir_from_int64(int64_t);
doremir_ptr_t doremir_from_float(float);
doremir_ptr_t doremir_from_double(double);

/** @}
    */

#endif // _DOREMIR

