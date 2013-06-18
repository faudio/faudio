
/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_bool(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_int8(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_int16(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_int32(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_int64(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_float(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_double(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_is_ref(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_to_bool(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int8_t fae_to_int8(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int16_t fae_to_int16(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int32_t fae_to_int32(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int64_t fae_to_int64(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
float fae_to_float(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
double fae_to_double(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
bool fae_peek_bool(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int8_t fae_peek_int8(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int16_t fae_peek_int16(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int32_t fae_peek_int32(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
int64_t fae_peek_int64(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
float fae_peek_float(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
double fae_peek_double(fae_ptr_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_bool(bool) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_int8(int8_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_int16(int16_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_int32(int32_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_int64(int64_t) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_float(float) {}

/**
    @see [Value references](@ref ValueReferences)
  */
fae_ptr_t fae_from_double(double) {}

/** Returns an implenentation of the given interface on the given value.
    @return Pointer to implementation (optional).
    @see [Interfaces](@ref Interfaces)
  */
fae_ptr_t fae_interface(fae_id_t, fae_ptr_t) {}

/** Return whether the given values are equal.
    @see [Equal](@ref fae_equal_t)
  */
bool fae_equal(fae_ptr_t, fae_ptr_t) {}

/** Return whether the given values are unequal.
    @see [Equal](@ref fae_equal_t)
  */
bool fae_not_equal(fae_ptr_t, fae_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fae_order_t)
  */
bool fae_less_than(fae_ptr_t, fae_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fae_order_t)
  */
bool fae_greater_than(fae_ptr_t, fae_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fae_order_t)
  */
bool fae_less_than_equal(fae_ptr_t, fae_ptr_t) {}

/** Return whether the given values have the given order.
    @see [Order](@ref fae_order_t)
  */
bool fae_greater_than_equal(fae_ptr_t, fae_ptr_t) {}

/** Return the less of the given values.
    @see [Order](@ref fae_order_t)
  */
fae_ptr_t fae_min(fae_ptr_t, fae_ptr_t) {}

/** Return the greater of the given values.
    @see [Order](@ref fae_order_t)
  */
fae_ptr_t fae_max(fae_ptr_t, fae_ptr_t) {}

/** Add the given values.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_add(fae_ptr_t, fae_ptr_t) {}

/** Subtract the given values.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_subtract(fae_ptr_t, fae_ptr_t) {}

/** Multiply the given values.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_multiply(fae_ptr_t, fae_ptr_t) {}

/** Divide the given values.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_divide(fae_ptr_t, fae_ptr_t) {}

/** Return the absolute of the given values.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_absolute(fae_ptr_t) {}

/** Add the given values. Destroys both arguments.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_dadd(fae_ptr_t, fae_ptr_t) {}

/** Subtract the given values. Destroys both arguments.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_dsubtract(fae_ptr_t, fae_ptr_t) {}

/** Multiply the given values. Destroys both arguments.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_dmultiply(fae_ptr_t, fae_ptr_t) {}

/** Divide the given values. Destroys both arguments.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_ddivide(fae_ptr_t, fae_ptr_t) {}

/** Return the absolute of the given values. Destroys both arguments.
    @see [Number](@ref fae_number_t)
  */
fae_ptr_t fae_dabsolute(fae_ptr_t) {}

/** Copy the given value.
    @see [Copy](@ref fae_copy_t)
  */
fae_ptr_t fae_copy(fae_ptr_t) {}

/** Move the given value (just return it).
  */
fae_ptr_t fae_move(fae_ptr_t) {}

/** Destroy the given value.
    @param  Value to destroy (destroyed).
    @see [Destroy](@ref fae_destroy_t)
  */
void fae_destroy(fae_ptr_t) {}

/** Recursively destroy the given value.
    @warning Not implemneted yet.
    @see [Destroy](@ref fae_destroy_t)
  */
void fae_deep_destroy(fae_ptr_t) {}

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref fae_error_log) as in:

    ~~~
    if (fae_check(value)) {
        fae_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
 */
bool fae_check(fae_ptr_t) {}

/** Print the given value, using [Show](@ref fae_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print.
  */
void fae_print(char *, fae_ptr_t) {}

/** Print the given value, using [Show](@ref fae_string_show_t).
    @param format   A printf-style format string.
    @param value    Value to print (destroyed).
  */
void fae_dprint(char *, fae_ptr_t) {}

