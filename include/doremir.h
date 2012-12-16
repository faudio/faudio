
#ifndef _DOREMIR
#define _DOREMIR

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    */

typedef void * doremir_ptr_t;
typedef doremir_ptr_t (* doremir_nullary_t)();
typedef doremir_ptr_t (* doremir_unary_t)(doremir_ptr_t);
typedef doremir_ptr_t (* doremir_binary_t)(doremir_ptr_t,
                                           doremir_ptr_t);
typedef bool (* doremir_pred_t)(doremir_ptr_t);
typedef struct {
            doremir_unary_t function; doremir_ptr_t value;
        } doremir_closure_t;
typedef struct {
            bool (* equal)(doremir_ptr_t, doremir_ptr_t);
        } doremir_equal_t;
typedef struct {
            bool (* less_than)(doremir_ptr_t, doremir_ptr_t);
            bool (* greater_than)(doremir_ptr_t, doremir_ptr_t);
        } doremir_order_t;
typedef struct {
            doremir_ptr_t (* add)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* subtract)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* multiply)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* divide)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* modulo)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* absolute)(doremir_ptr_t, doremir_ptr_t);
        } doremir_number_t;
bool doremir_to_bool(doremir_ptr_t);
int doremir_to_int(doremir_ptr_t);
int8_t doremir_to_int8(doremir_ptr_t);
int16_t doremir_to_int16(doremir_ptr_t);
int32_t doremir_to_int32(doremir_ptr_t);
float doremir_to_float(doremir_ptr_t);
double doremir_to_double(doremir_ptr_t);
doremir_ptr_t doremir_from_bool(bool);
doremir_ptr_t doremir_from_int(int);
doremir_ptr_t doremir_from_int8(int8_t);
doremir_ptr_t doremir_from_int16(int16_t);
doremir_ptr_t doremir_from_int32(int32_t);
doremir_ptr_t doremir_from_float(float);
doremir_ptr_t doremir_from_double(double);
doremir_ptr_t doremir_copy(doremir_ptr_t);
doremir_ptr_t doremir_move(doremir_ptr_t);
void doremir_destroy(doremir_ptr_t);
typedef int64_t doremir_id_t;
typedef doremir_ptr_t (* doremir_impl_t)(doremir_id_t);
doremir_ptr_t doremir_interface(doremir_id_t, doremir_ptr_t);

/** @}
    */

#endif // _DOREMIR

