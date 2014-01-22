
#ifndef _FA
#define _FA

#include <fa/std.h>
#include <fa/alloc.h>
#include <fa/interfaces.h>

/** @addtogroup Fa

    Basic definitions. 
 
    @defgroup Fa Fa
    @{
    */

/** Pointer type, equivalent to `void*`.
    
*/
typedef void * fa_ptr_t;

/** A nullary function, defined as `fa_ptr_t(*fa_nullary_t )(fa_ptr_t)`.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_nullary_t)(fa_ptr_t);

/** A unary function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_unary_t)(fa_ptr_t, fa_ptr_t);

/** A binary function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_binary_t)(fa_ptr_t, fa_ptr_t, fa_ptr_t);

/** A ternary function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef fa_ptr_t (* fa_ternary_t)(fa_ptr_t,
                                  fa_ptr_t,
                                  fa_ptr_t,
                                  fa_ptr_t);

/** A predicate, or boolean function.

    The first argument is an environment pointer which is usually passed along with the function.
    This is the standard C technique for closing over variables.
    
*/
typedef bool (* fa_pred_t)(fa_ptr_t, fa_ptr_t);

/** An 8-bit character. 
*/
typedef char fa_char8_t;

/** A 16-bit character. 
*/
typedef uint16_t fa_char16_t;

/** A 32-bit character. 
*/
typedef uint32_t fa_char32_t;

/**
    @see [Value references](@ref ValueReferences)
      
*/
bool fa_is_bool(fa_ptr_t ptr);


bool fa_is_int8(fa_ptr_t ptr);


bool fa_is_int16(fa_ptr_t ptr);


bool fa_is_int32(fa_ptr_t ptr);


bool fa_is_int64(fa_ptr_t ptr);


bool fa_is_float(fa_ptr_t ptr);


bool fa_is_double(fa_ptr_t ptr);


bool fa_is_ref(fa_ptr_t ptr);


bool fa_to_bool(fa_ptr_t ptr);


int8_t fa_to_int8(fa_ptr_t ptr);


int16_t fa_to_int16(fa_ptr_t ptr);


int32_t fa_to_int32(fa_ptr_t ptr);


int64_t fa_to_int64(fa_ptr_t ptr);


float fa_to_float(fa_ptr_t ptr);


double fa_to_double(fa_ptr_t ptr);


bool fa_peek_bool(fa_ptr_t ptr);


int8_t fa_peek_int8(fa_ptr_t ptr);


int16_t fa_peek_int16(fa_ptr_t ptr);


int32_t fa_peek_int32(fa_ptr_t ptr);


int64_t fa_peek_int64(fa_ptr_t ptr);


float fa_peek_float(fa_ptr_t ptr);


double fa_peek_double(fa_ptr_t ptr);


fa_ptr_t fa_from_bool(bool bool);


fa_ptr_t fa_from_int8(int8_t int8);


fa_ptr_t fa_from_int16(int16_t int16);


fa_ptr_t fa_from_int32(int32_t int32);


fa_ptr_t fa_from_int64(int64_t int64);


fa_ptr_t fa_from_float(float float);


fa_ptr_t fa_from_double(double double);

/** Unique identifier. Only used for interface lookup at the moment.
    
*/
typedef int64_t fa_id_t;

/** Callback to lookup an interface implementation.
    
*/
typedef fa_ptr_t (* fa_impl_t)(fa_id_t);

/** Returns an implenentation of the given interface on the given value.
    @return Pointer to implementation (optional).
    @see [Interfaces](@ref Interfaces)
      
*/
fa_ptr_t fa_interface(fa_id_t id, fa_ptr_t ptr);

/** Equality comparison interface.
    
*/
typedef struct {
            bool (* equal)(fa_ptr_t, fa_ptr_t);
        } fa_equal_t;

/** Return whether the given values are equal.
    @see [Equal](@ref fa_equal_t)
      
*/
bool fa_equal(fa_ptr_t ptr, fa_ptr_t ptr);

/** Return whether the given values are unequal.
    @see [Equal](@ref fa_equal_t)
      
*/
bool fa_not_equal(fa_ptr_t ptr, fa_ptr_t ptr);

/** Less-than comparison interface.
    
*/
typedef struct {
            bool (* less_than)(fa_ptr_t, fa_ptr_t);
            bool (* greater_than)(fa_ptr_t, fa_ptr_t);
        } fa_order_t;


bool fa_less_than(fa_ptr_t ptr, fa_ptr_t ptr);


bool fa_greater_than(fa_ptr_t ptr, fa_ptr_t ptr);


bool fa_less_than_equal(fa_ptr_t ptr, fa_ptr_t ptr);


bool fa_greater_than_equal(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_min(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_max(fa_ptr_t ptr, fa_ptr_t ptr);

/** Arithmetic operations interface.
    
*/
typedef struct {
            fa_ptr_t (* add)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* subtract)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* multiply)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* divide)(fa_ptr_t, fa_ptr_t);
            fa_ptr_t (* absolute)(fa_ptr_t);
        } fa_number_t;


fa_ptr_t fa_add(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_subtract(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_multiply(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_divide(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_absolute(fa_ptr_t ptr);


fa_ptr_t fa_dadd(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_dsubtract(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_dmultiply(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_ddivide(fa_ptr_t ptr, fa_ptr_t ptr);


fa_ptr_t fa_dabsolute(fa_ptr_t ptr);

/** Generic copying interface.
    
*/
typedef struct {
            fa_ptr_t (* copy)(fa_ptr_t);
        } fa_copy_t;

/** Generic destruction interface.
    
*/
typedef struct {
            void (* destroy)(fa_ptr_t);
        } fa_destroy_t;

/** Copy the given value.
    @see [Copy](@ref fa_copy_t)
      
*/
fa_ptr_t fa_copy(fa_ptr_t ptr);

/** Move the given value. This is the identity function,
    just serves as a notification.
      
*/
fa_ptr_t fa_move(fa_ptr_t ptr);

/** Destroy the given value.
    @param  Value to destroy (destroyed).
    @see [Destroy](@ref fa_destroy_t)
      
*/
void fa_destroy(fa_ptr_t ptr);

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref fa_error_log) as in:

    ~~~
    if (fa_check(value)) {
        fa_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
*/
bool fa_check(fa_ptr_t ptr);

/** Print the given value, using [Show](@ref fa_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print.
      
*/
void fa_print(char *, fa_ptr_t ptr);


void fa_dprint(char *, fa_ptr_t ptr);


void fa_print_ln(fa_ptr_t ptr);


void fa_dprint_ln(fa_ptr_t ptr);

/** @}
    */

#endif // _FA

