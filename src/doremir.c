
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir.h>
#include <doremir/string.h>

#pragma GCC diagnostic ignored "-Wparentheses"

/*
    doremir_ptr_t

    v = value
    _ = anything

    Layout                                  Tag
    ===================================     ===

    bool
    _______v ________ ________ _____111     0x7
    int8
    vvvvvvvv ________ ________ _____110     0x6
    int16
    vvvvvvvv vvvvvvvv ________ _____101     0x5
    boxed int32
    vvvvvvvv vvvvvvvv vvvvvvvv vvvvv100     0x4
    boxed int64
    vvvvvvvv vvvvvvvv vvvvvvvv vvvvv011     0x3
    boxed float
    vvvvvvvv vvvvvvvv vvvvvvvv vvvvv010     0x2
    boxed double
    vvvvvvvv vvvvvvvv vvvvvvvv vvvvv001     0x1
    ptr
    vvvvvvvv vvvvvvvv vvvvvvvv vvvvv000     0x0

    Note: ptr includes 0 (NULL)
 */

int doremir_type(doremir_ptr_t a)
{
    return ((intptr_t) a) & 0x7;
}

char * doremir_type_str(doremir_ptr_t a)
{
    switch (doremir_type(a)) {
        case 7:
            return "bool";

        case 6:
            return "int8";

        case 5:
            return "int16";

        case 4:
            return "int32";

        case 3:
            return "int64";

        case 2:
            return "float";

        case 1:
            return "double";

        case 0:
            return "ptr";

        default:
            return "unknown";
    }
}

bool doremir_is_bool(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x7;
}
bool doremir_is_int8(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x6;
}
bool doremir_is_int16(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x5;
}
bool doremir_is_int32(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x4;
}
bool doremir_is_int64(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x3;
}
bool doremir_is_float(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x2;
}
bool doremir_is_double(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x1;
}
bool doremir_is_ref(doremir_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x0;
}


// --------------------------------------------------------------------------------
// Wrapper functions
// --------------------------------------------------------------------------------

bool doremir_to_bool(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x7 && "Wrong type, expected bool");
    return (p & ~0x7) >> 24;
}

doremir_ptr_t doremir_from_bool(bool a)
{
    return (doremir_ptr_t)(a << 24 & ~0x7 | 0x7);
}

// --------------------------------------------------------------------------------

int8_t doremir_to_int8(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x6 && "Wrong type, expected int8");
    return (p & ~0x7) >> 24;
}

doremir_ptr_t doremir_from_int8(int8_t a)
{
    return (doremir_ptr_t)(a << 24 & ~0x7 | 0x6);
}

// --------------------------------------------------------------------------------

int16_t doremir_to_int16(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x5 && "Wrong type, expected int16");
    return (p & ~0x7) >> 8;
}

doremir_ptr_t doremir_from_int16(int16_t a)
{
    return (doremir_ptr_t)(a << 8 & ~0x7 | 0x5);
}

// --------------------------------------------------------------------------------

int32_t doremir_peek_int32(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x4 && "Wrong type, expected int32");
    int32_t v = *((int32_t *)(p & ~0x7));
    return v;
}

int32_t doremir_to_int32(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x4 && "Wrong type, expected int32");
    int32_t v = *((int32_t *)(p & ~0x7));
    free((int32_t *)(p & ~0x7));
    return v;
}

doremir_ptr_t doremir_copy_int32(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    int32_t * q = malloc(sizeof(int32_t));
    *q = *((int32_t *)(p & ~0x7));
    return (doremir_ptr_t)(((intptr_t) q) | 0x4);
}

doremir_ptr_t doremir_from_int32(int32_t a)
{
    int32_t * p = malloc(sizeof(int32_t));
    *p = a;
    return (doremir_ptr_t)(((intptr_t) p) & ~0x7 | 0x4);
}

// --------------------------------------------------------------------------------

int64_t doremir_peek_int64(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x3 && "Wrong type, expected int64");
    int64_t v = *((int64_t *)(p & ~0x7));
    return v;
}

int64_t doremir_to_int64(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x3 && "Wrong type, expected int64");
    int64_t v = *((int64_t *)(p & ~0x7));
    free((int64_t *)(p & ~0x7));
    return v;
}

doremir_ptr_t doremir_copy_int64(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    int64_t * q = malloc(sizeof(int64_t));
    *q = *((int64_t *)(p & ~0x7));
    return (doremir_ptr_t)(((intptr_t) q) & ~0x7 | 0x3);
}

doremir_ptr_t doremir_from_int64(int64_t a)
{
    int64_t * p = malloc(sizeof(int64_t));
    *p = a;
    return (doremir_ptr_t)(((intptr_t) p) & ~0x7 | 0x3);
}

// --------------------------------------------------------------------------------

float doremir_peek_float(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x2 && "Wrong type, expected float");
    float v = *((float *)(p & ~0x7));
    return v;
}
float doremir_to_float(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x2 && "Wrong type, expected float");
    float v = *((float *)(p & ~0x7));
    free((float *)(p & ~0x7));
    return v;
}

doremir_ptr_t doremir_copy_float(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    float * q = malloc(sizeof(float));
    *q = *((float *)(p & ~0x7));
    return (doremir_ptr_t)(((intptr_t) q) & ~0x7 | 0x2);
}

doremir_ptr_t doremir_from_float(float a)
{
    float * p = malloc(sizeof(float));
    *p = a;
    return (doremir_ptr_t)(((intptr_t) p) & ~0x7 | 0x2);
}

// --------------------------------------------------------------------------------

double doremir_peek_double(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x1 && "Wrong type, expected double");
    double v = *((double *)(p & ~0x7));
    return v;
}
double doremir_to_double(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x1 && "Wrong type, expected double");
    double v = *((double *)(p & ~0x7));
    free((double *)(p & ~0x7));
    return v;
}

doremir_ptr_t doremir_copy_double(doremir_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    double * q = malloc(sizeof(double));
    *q = *((double *)(p & ~0x7));
    return (doremir_ptr_t)(((intptr_t) q) & ~0x7 | 0x1);
}

doremir_ptr_t doremir_from_double(double a)
{
    double * p = malloc(sizeof(double));
    *p = a;
    return (doremir_ptr_t)(((intptr_t) p) & ~0x7 | 0x1);
}

// --------------------------------------------------------------------------------
// Generic functions
// --------------------------------------------------------------------------------

#define GENERIC1(I,F,A,B) \
    B doremir_##F(A a)                                                                      \
    {                                                                                       \
        assert(doremir_interface(doremir_##I##_i, a) && "Must implement " #I);              \
        return ((doremir_##I##_t*) doremir_interface(doremir_##I##_i, a))->F(a);            \
    }

#define GENERIC2(I,F,A,B,C) \
    C doremir_##F(A a, B b)                                                                 \
    {                                                                                       \
        assert(doremir_interface(doremir_##I##_i, a) && "Must implement " #I);              \
        return ((doremir_##I##_t*) doremir_interface(doremir_##I##_i, a))->F(a, b);         \
    }


GENERIC2(equal,     equal,          doremir_ptr_t, doremir_ptr_t, bool);
GENERIC2(order,     less_than,      doremir_ptr_t, doremir_ptr_t, bool);
GENERIC2(order,     greater_than,   doremir_ptr_t, doremir_ptr_t, bool);

bool doremir_not_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    return !doremir_equal(a, b);
}

bool doremir_less_than_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_less_than(a, b) || doremir_equal(a, b);
}

bool doremir_greater_than_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_greater_than(a, b) || doremir_equal(a, b);
}

doremir_ptr_t doremir_min(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_less_than(a, b) ? a : b;
}

doremir_ptr_t doremir_max(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_greater_than(a, b) ? a : b;
}


GENERIC2(number,    add,            doremir_ptr_t, doremir_ptr_t, doremir_ptr_t);
GENERIC2(number,    subtract,       doremir_ptr_t, doremir_ptr_t, doremir_ptr_t);
GENERIC2(number,    multiply,       doremir_ptr_t, doremir_ptr_t, doremir_ptr_t);
GENERIC2(number,    divide,         doremir_ptr_t, doremir_ptr_t, doremir_ptr_t);
GENERIC1(number,    absolute,       doremir_ptr_t, doremir_ptr_t);

GENERIC1(copy,      copy,           doremir_ptr_t, doremir_ptr_t);
GENERIC1(destroy,   destroy,        doremir_ptr_t, void);

doremir_string_t doremir_show(doremir_ptr_t a)
{
    return doremir_string_show(a);
}

void doremir_print(char * f, doremir_ptr_t a)
{
    if (a) {
        doremir_string_t str = doremir_string_show(a);
        char * cstr = doremir_string_to_utf8(str);
        printf(f, cstr);
        free(cstr);
        doremir_destroy(str);
    } else {
        printf("%s", f);
    }
}

void doremir_dprint(char * f, doremir_ptr_t a)
{
    doremir_print(f, a);
    doremir_destroy(a);
}

void doremir_print_ln(doremir_ptr_t a)
{
    doremir_print("%s\n", a);
}

void doremir_dprint_ln(doremir_ptr_t a)
{
    doremir_dprint("%s\n", a);
}


doremir_ptr_t doremir_move(doremir_ptr_t a)
{
    return a;
}

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
bool doremir_check(doremir_ptr_t a)
{
    bool doremir_error_check(doremir_ptr_t a);
    return doremir_error_check(a);
}

// --------------------------------------------------------------------------------
// Wrapped implementations
// --------------------------------------------------------------------------------

/* Generates T_impl
 */
#define IMPLEMENT_WRAPPER(T) \
    doremir_ptr_t T##_impl(doremir_id_t interface)                                          \
    {                                                                                       \
        static doremir_equal_t   T##_equal_impl   =                                         \
            { T##_equal };                                                                  \
        static doremir_order_t   T##_order_impl   =                                         \
            { T##_less_than, T##_greater_than };                                            \
        static doremir_number_t  T##_number_impl  =                                         \
            { T##_add, T##_subtract, T##_multiply, T##_divide, T##_absolute };              \
        static doremir_string_show_t    T##_show_impl    =                                  \
            { T##_show };                                                                   \
        static doremir_copy_t    T##_copy_impl    =                                         \
            { T##_copy };                                                                   \
        static doremir_destroy_t T##_destroy_impl =                                         \
            { T##_destroy };                                                                \
                                                                                            \
        switch (interface)                                                                  \
        {                                                                                   \
        case doremir_equal_i:                                                               \
            return &T##_equal_impl;                                                         \
        case doremir_order_i:                                                               \
            return &T##_order_impl;                                                         \
        case doremir_number_i:                                                              \
            return &T##_number_impl;                                                        \
        case doremir_string_show_i:                                                         \
            return &T##_show_impl;                                                          \
        case doremir_copy_i:                                                                \
            return &T##_copy_impl;                                                          \
        case doremir_destroy_i:                                                             \
            return &T##_destroy_impl;                                                       \
        default:                                                                            \
            return NULL;                                                                    \
        }                                                                                   \
    }

#define UNBOXED_WRAPPER_IMPL(T) \
    bool T##_equal(doremir_ptr_t a, doremir_ptr_t b)                                        \
    {                                                                                       \
        return (doremir_to_##T(a) == doremir_to_##T(b));                                    \
    }                                                                                       \
    bool T##_less_than(doremir_ptr_t a, doremir_ptr_t b)                                    \
    {                                                                                       \
        return (doremir_to_##T(a) < doremir_to_##T(b));                                     \
    }                                                                                       \
    bool T##_greater_than(doremir_ptr_t a, doremir_ptr_t b)                                 \
    {                                                                                       \
        return (doremir_to_##T(a) > doremir_to_##T(b));                                     \
    }                                                                                       \
    doremir_ptr_t T##_add(doremir_ptr_t a, doremir_ptr_t b)                                 \
    {                                                                                       \
        return doremir_from_##T(doremir_to_##T(a) + doremir_to_##T(b));                     \
    }                                                                                       \
    doremir_ptr_t T##_subtract(doremir_ptr_t a, doremir_ptr_t b)                            \
    {                                                                                       \
        return doremir_from_##T(doremir_to_##T(a) - doremir_to_##T(b));                     \
    }                                                                                       \
    doremir_ptr_t T##_multiply(doremir_ptr_t a, doremir_ptr_t b)                            \
    {                                                                                       \
        return doremir_from_##T(doremir_to_##T(a) * doremir_to_##T(b));                     \
    }                                                                                       \
    doremir_ptr_t T##_divide(doremir_ptr_t a, doremir_ptr_t b)                              \
    {                                                                                       \
        return doremir_from_##T(doremir_to_##T(a) / doremir_to_##T(b));                     \
    }                                                                                       \
    doremir_ptr_t T##_absolute(doremir_ptr_t a)                                             \
    {                                                                                       \
        return doremir_from_##T(abs(doremir_to_##T(a)));                                    \
    }                                                                                       \
    doremir_ptr_t T##_copy(doremir_ptr_t a)                                                 \
    {                                                                                       \
        return a;                                                                           \
    }                                                                                       \
    void T##_destroy(doremir_ptr_t a)                                                       \
    {                                                                                       \
        /* nothing to do */                                                                 \
    }

#define BOXED_WRAPPER_IMPL(T) \
    bool T##_equal(doremir_ptr_t a, doremir_ptr_t b)                                        \
    {                                                                                       \
        return (doremir_peek_##T(a) == doremir_peek_##T(b));                                \
    }                                                                                       \
    bool T##_less_than(doremir_ptr_t a, doremir_ptr_t b)                                    \
    {                                                                                       \
        return (doremir_peek_##T(a) < doremir_peek_##T(b));                                 \
    }                                                                                       \
    bool T##_greater_than(doremir_ptr_t a, doremir_ptr_t b)                                 \
    {                                                                                       \
        return (doremir_peek_##T(a) > doremir_peek_##T(b));                                 \
    }                                                                                       \
    doremir_ptr_t T##_add(doremir_ptr_t a, doremir_ptr_t b)                                 \
    {                                                                                       \
        return doremir_from_##T(doremir_peek_##T(a) + doremir_peek_##T(b));                 \
    }                                                                                       \
    doremir_ptr_t T##_subtract(doremir_ptr_t a, doremir_ptr_t b)                            \
    {                                                                                       \
        return doremir_from_##T(doremir_peek_##T(a) - doremir_peek_##T(b));                 \
    }                                                                                       \
    doremir_ptr_t T##_multiply(doremir_ptr_t a, doremir_ptr_t b)                            \
    {                                                                                       \
        return doremir_from_##T(doremir_peek_##T(a) * doremir_peek_##T(b));                 \
    }                                                                                       \
    doremir_ptr_t T##_divide(doremir_ptr_t a, doremir_ptr_t b)                              \
    {                                                                                       \
        return doremir_from_##T(doremir_peek_##T(a) / doremir_peek_##T(b));                 \
    }                                                                                       \
    doremir_ptr_t T##_absolute(doremir_ptr_t a)                                             \
    {                                                                                       \
        return doremir_from_##T(abs(doremir_peek_##T(a))); /* TODO use tg? */               \
    }                                                                                       \
    doremir_ptr_t T##_copy(doremir_ptr_t a)                                                 \
    {                                                                                       \
        return doremir_copy_##T(a);                                                         \
    }                                                                                       \
    void T##_destroy(doremir_ptr_t a)                                                       \
    {                                                                                       \
        doremir_to_##T(a);                                                                  \
    }

#define UNBOXED_SHOW_IMPL(T,F) \
    doremir_string_t T##_show(doremir_ptr_t a)                                              \
    {                                                                                       \
        int  n;                                                                             \
        char cs[16];                                                                        \
        n = snprintf(cs, 16, F, doremir_to_##T(a));                                         \
        cs[n] = 0; /* terminate */                                                          \
        return doremir_string_from_utf8(cs);                                                \
    }

#define BOXED_SHOW_IMPL(T,F) \
    doremir_string_t T##_show(doremir_ptr_t a)                                              \
    {                                                                                       \
        int  n;                                                                             \
        char cs[16];                                                                        \
        n = snprintf(cs, 16, F, doremir_peek_##T(a));                                       \
        cs[n] = 0; /* terminate */                                                          \
        return doremir_string_from_utf8(cs);                                                \
    }

UNBOXED_WRAPPER_IMPL(bool);
UNBOXED_WRAPPER_IMPL(int8);
UNBOXED_WRAPPER_IMPL(int16);
BOXED_WRAPPER_IMPL(int32);
BOXED_WRAPPER_IMPL(int64);
BOXED_WRAPPER_IMPL(float);
BOXED_WRAPPER_IMPL(double);

doremir_string_t bool_show(doremir_ptr_t a)
{
    return doremir_to_bool(a) ? doremir_string_from_utf8("true")
           : doremir_string_from_utf8("false");
}

UNBOXED_SHOW_IMPL(int8, "%i");
UNBOXED_SHOW_IMPL(int16, "%i");
BOXED_SHOW_IMPL(int32, "%i");
BOXED_SHOW_IMPL(int64, "%" PRId64);
BOXED_SHOW_IMPL(float, "%f");
BOXED_SHOW_IMPL(double, "%f");


IMPLEMENT_WRAPPER(bool);
IMPLEMENT_WRAPPER(int8);
IMPLEMENT_WRAPPER(int16);
IMPLEMENT_WRAPPER(int32);
IMPLEMENT_WRAPPER(int64);
IMPLEMENT_WRAPPER(float);
IMPLEMENT_WRAPPER(double);


// @cond internal
// Dummy struct as clang (C99?) does not allow us to call the pointer directly
struct doremir_impl_disp {
    doremir_impl_t impl;
};
// @endcond

doremir_ptr_t doremir_interface(doremir_id_t type, doremir_ptr_t pointer)
{
    switch (doremir_type(pointer)) {
        case 7:
            return bool_impl(type);

        case 6:
            return int8_impl(type);

        case 5:
            return int16_impl(type);

        case 4:
            return int32_impl(type);

        case 3:
            return int64_impl(type);

        case 2:
            return float_impl(type);

        case 1:
            return double_impl(type);

        default:
            //  If you get a bus error here, you probably passed a
            //  non-boxed primitive to a generic function.
            return ((struct doremir_impl_disp *) pointer)->impl(type);
    }
}
