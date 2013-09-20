
/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_bool(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_int8(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_int16(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_int32(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_int64(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_float(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_double(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_is_ref(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_to_bool(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int8_t fa_to_int8(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int16_t fa_to_int16(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int32_t fa_to_int32(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int64_t fa_to_int64(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
float fa_to_float(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
double fa_to_double(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fa_peek_bool(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int8_t fa_peek_int8(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int16_t fa_peek_int16(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int32_t fa_peek_int32(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int64_t fa_peek_int64(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
float fa_peek_float(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
double fa_peek_double(fa_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_bool(bool) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_int8(int8_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_int16(int16_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_int32(int32_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_int64(int64_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_float(float) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fa_ptr_t fa_from_double(double) {}

/** Returns an implenentation of the given interface on the given value.
    @return Pointer to implementation (optional).
    @see [Interfaces](@ref Interfaces)
  */
fa_ptr_t fa_interface(fa_id_t, fa_ptr_t) {}

/** Return whether the given values are equal.
    @see [Equal](@ref fa_equal_t)
  */
bool fa_equal(fa_ptr_t, fa_ptr_t) {}

/** Return whether the given values are unequal.
    @see [Equal](@ref fa_equal_t)
  */
bool fa_not_equal(fa_ptr_t, fa_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fa_order_t)
  */
bool fa_less_than(fa_ptr_t, fa_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fa_order_t)
  */
bool fa_greater_than(fa_ptr_t, fa_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fa_order_t)
  */
bool fa_less_than_equal(fa_ptr_t, fa_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fa_order_t)
  */
bool fa_greater_than_equal(fa_ptr_t, fa_ptr_t) {}

/** Return the less of the given values.
    @see [Order](@ref fa_order_t)
  */
fa_ptr_t fa_min(fa_ptr_t, fa_ptr_t) {}

/** Return the greater of the given values.
    @see [Order](@ref fa_order_t)
  */
fa_ptr_t fa_max(fa_ptr_t, fa_ptr_t) {}

/** Add the given values.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_add(fa_ptr_t, fa_ptr_t) {}

/** Subtract the given values.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_subtract(fa_ptr_t, fa_ptr_t) {}

/** Multiply the given values.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_multiply(fa_ptr_t, fa_ptr_t) {}

/** Divide the given values.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_divide(fa_ptr_t, fa_ptr_t) {}

/** Return the absolute of the given values.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_absolute(fa_ptr_t) {}

/** Add the given values. Destroys both arguments.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_dadd(fa_ptr_t, fa_ptr_t) {}

/** Subtract the given values. Destroys both arguments.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_dsubtract(fa_ptr_t, fa_ptr_t) {}

/** Multiply the given values. Destroys both arguments.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_dmultiply(fa_ptr_t, fa_ptr_t) {}

/** Divide the given values. Destroys both arguments.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_ddivide(fa_ptr_t, fa_ptr_t) {}

/** Return the absolute of the given values. Destroys both arguments.
    @see [Number](@ref fa_number_t)
  */
fa_ptr_t fa_dabsolute(fa_ptr_t) {}

/** Copy the given value.
    @see [Copy](@ref fa_copy_t)
  */
fa_ptr_t fa_copy(fa_ptr_t) {}

/** Move the given value (just return it).
  */
fa_ptr_t fa_move(fa_ptr_t) {}

/** Destroy the given value.
    @param  Value to destroy (destroyed).
    @see [Destroy](@ref fa_destroy_t)
  */
void fa_destroy(fa_ptr_t) {}

/** Recursively destroy the given value.
    @warning Not implemneted yet.
    @see [Destroy](@ref fa_destroy_t)
  */
void fa_deep_destroy(fa_ptr_t) {}

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
bool fa_check(fa_ptr_t) {}

/** Print the given value, using [Show](@ref fa_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print.
  */
void fa_print(char *, fa_ptr_t) {}

/** Print the given value, using [Show](@ref fa_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print (destroyed).
  */
void fa_dprint(char *, fa_ptr_t) {}

