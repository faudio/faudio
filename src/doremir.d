
/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_bool(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_int8(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_int16(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_int32(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_int64(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_float(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_double(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_is_ref(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_to_bool(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int8_t doremir_to_int8(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int16_t doremir_to_int16(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int32_t doremir_to_int32(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int64_t doremir_to_int64(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
float doremir_to_float(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
double doremir_to_double(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
bool doremir_peek_bool(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int8_t doremir_peek_int8(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int16_t doremir_peek_int16(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int32_t doremir_peek_int32(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
int64_t doremir_peek_int64(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
float doremir_peek_float(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
double doremir_peek_double(doremir_ptr_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_bool(bool) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_int8(int8_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_int16(int16_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_int32(int32_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_int64(int64_t) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_float(float) {}

/** 
    @see [Value references](@ref ValueReferences)
  */
doremir_ptr_t doremir_from_double(double) {}

/** Returns an implenentation of the given interface on the given value.
    @return Pointer to implementation (optional).
    @see [Interfaces](@ref Interfaces)
  */
doremir_ptr_t doremir_interface(doremir_id_t, doremir_ptr_t) {}

/** Return whether the given values are equal.
    @see [Equal](@ref doremir_equal_t)
  */
bool doremir_equal(doremir_ptr_t, doremir_ptr_t) {}

/** Return whether the given values are unequal.
    @see [Equal](@ref doremir_equal_t)
  */
bool doremir_not_equal(doremir_ptr_t, doremir_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref doremir_order_t)
  */
bool doremir_less_than(doremir_ptr_t, doremir_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref doremir_order_t)
  */
bool doremir_greater_than(doremir_ptr_t, doremir_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref doremir_order_t)
  */
bool doremir_less_than_equal(doremir_ptr_t, doremir_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref doremir_order_t)
  */
bool doremir_greater_than_equal(doremir_ptr_t, doremir_ptr_t) {}

/** Return the less of the given values.
    @see [Order](@ref doremir_order_t)
  */
doremir_ptr_t doremir_min(doremir_ptr_t, doremir_ptr_t) {}

/** Return the greater of the given values.
    @see [Order](@ref doremir_order_t)
  */
doremir_ptr_t doremir_max(doremir_ptr_t, doremir_ptr_t) {}

/** Add the given values.
    @see [Number](@ref doremir_number_t)
  */
doremir_ptr_t doremir_add(doremir_ptr_t, doremir_ptr_t) {}

/** Subtract the given values.
    @see [Number](@ref doremir_number_t)
  */
doremir_ptr_t doremir_subtract(doremir_ptr_t, doremir_ptr_t) {}

/** Multiply the given values.
    @see [Number](@ref doremir_number_t)
  */
doremir_ptr_t doremir_multiply(doremir_ptr_t, doremir_ptr_t) {}

/** Divide the given values.
    @see [Number](@ref doremir_number_t)
  */
doremir_ptr_t doremir_divide(doremir_ptr_t, doremir_ptr_t) {}

/** Return the absolute of the given values.
    @see [Number](@ref doremir_number_t)
  */
doremir_ptr_t doremir_absolute(doremir_ptr_t) {}

/** Copy the given value.
    @see [Copy](@ref doremir_copy_t)
  */
doremir_ptr_t doremir_copy(doremir_ptr_t) {}

/** Move the given value (just return it).
  */
doremir_ptr_t doremir_move(doremir_ptr_t) {}

/** Destroy the given value.
    @param  Value to destroy (destroyed).
    @see [Destroy](@ref doremir_destroy_t)
  */
void doremir_destroy(doremir_ptr_t) {}

/** Recursively destroy the given value.
    @warning Not implemneted yet.
    @see [Destroy](@ref doremir_destroy_t)
  */
void doremir_deep_destroy(doremir_ptr_t) {}

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref doremir_error_log) as in:

    ~~~
    if (doremir_check(value)) {
        doremir_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
 */
bool doremir_check(doremir_ptr_t) {}

/** Print the given value, using [Show](@ref doremir_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print.
  */
void doremir_print(char *, doremir_ptr_t) {}

/** Print the given value, using [Show](@ref doremir_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print (destroyed).
  */
void doremir_dprint(char *, doremir_ptr_t) {}

