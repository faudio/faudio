
#ifndef _FA
#define _FA

#include <fa/std.h>
#include <fa/alloc.h>
#include <fa/interfaces.h>

/** @addtogroup Fa

    @addtogroup Fa

    Core definitions. Import @ref FaAudioEngine instead of this module.

 
    @defgroup Fa Fa
    @{
    */

/** @typedef fa_ptr_t
    Pointer type, equivalent to `void*`.
    
*/
typedef void * fa_ptr_t;

/** @typedef fa_nullary_t
    A nullary function, defined as `fa_ptr_t(*fa_nullary_t )(fa_ptr_t)`.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_nullary_t)(fa_ptr_t);

/** @typedef fa_unary_t
    A unary function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_unary_t)(fa_ptr_t, fa_ptr_t);

/** @typedef fa_binary_t
    A binary function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_binary_t)(fa_ptr_t, fa_ptr_t, fa_ptr_t);

/** @typedef fa_ternary_t
    A ternary function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_ternary_t)(fa_ptr_t,
                                  fa_ptr_t,
                                  fa_ptr_t,
                                  fa_ptr_t);

/** @typedef fa_pred_t
    A predicate, or boolean function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef bool (* fa_pred_t)(fa_ptr_t, fa_ptr_t);

/**
    @typedef fa_char8_t
    An 8-bit character.

    @typedef fa_char16_t
    A 16-bit character.

    @typedef fa_char32_t
    A 32-bit character.
*/
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

/** @typedef fa_id_t
    Unique identifier. Only used for interface lookup at the moment.
    
*/
typedef int64_t fa_id_t;

/** @typedef fa_impl_t
    Callback to lookup an interface implementation.
    
*/
typedef fa_ptr_t (* fa_impl_t)(fa_id_t);


fa_ptr_t fa_interface(fa_id_t, fa_ptr_t);

/** @interface fa_equal_t
    Equality comparison interface.
    
*/
typedef struct {
            bool (* equal)(fa_ptr_t, fa_ptr_t);
        } fa_equal_t;


bool fa_equal(fa_ptr_t, fa_ptr_t);


bool fa_not_equal(fa_ptr_t, fa_ptr_t);

/** @interface fa_order_t
    Less-than comparison interface.
    
*/
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

/** @interface fa_number_t
    Arithmetic operations interface.
    
*/
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

/** @interface fa_copy_t
    Generic copying interface.
    
*/
typedef struct {
            fa_ptr_t (* copy)(fa_ptr_t);
        } fa_copy_t;

/** @interface fa_destroy_t
    Generic destruction interface.
    
*/
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

