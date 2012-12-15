
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
            bool (* eq)(doremir_ptr_t, doremir_ptr_t);
        } doremir_eq_t;
typedef struct {
            bool (* lt)(doremir_ptr_t, doremir_ptr_t);
            bool (* gt)(doremir_ptr_t, doremir_ptr_t);
        } doremir_ord_t;
typedef struct {
            doremir_ptr_t (* add)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* sub)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* mul)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* div)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* mod)(doremir_ptr_t, doremir_ptr_t);
            doremir_ptr_t (* abs)(doremir_ptr_t, doremir_ptr_t);
        } doremir_num_t;
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
doremir_ptr_t doremir_get_interface(int64_t, doremir_ptr_t);

/** @}
    */

#endif // _DOREMIR

