
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae.h>
#include <fae/dynamic.h>
#include <fae/string.h>

#pragma GCC diagnostic ignored "-Wparentheses"

/*
    fae_ptr_t

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

int fae_type(fae_ptr_t a)
{
    return ((intptr_t) a) & 0x7;
}

char *fae_type_str(fae_ptr_t a)
{
    switch (fae_type(a)) {
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

bool fae_is_bool(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x7;
}
bool fae_is_int8(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x6;
}
bool fae_is_int16(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x5;
}
bool fae_is_int32(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x4;
}
bool fae_is_int64(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x3;
}
bool fae_is_float(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x2;
}
bool fae_is_double(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x1;
}
bool fae_is_ref(fae_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x0;
}

// fae_dynamic_type_repr_t bool_get_type(fae_ptr_t a)
// {
//     bool_type_repr;
// }
// fae_dynamic_type_repr_t int8_get_type(fae_ptr_t a)
// {
//     return i8_type_repr;
// }
// fae_dynamic_type_repr_t int16_get_type(fae_ptr_t a)
// {
//     return i16_type_repr;
// }
// fae_dynamic_type_repr_t int32_get_type(fae_ptr_t a)
// {
//     return i32_type_repr;
// }
// fae_dynamic_type_repr_t int64_get_type(fae_ptr_t a)
// {
//     return i64_type_repr;
// }
// fae_dynamic_type_repr_t float_get_type(fae_ptr_t a)
// {
//     return f32_type_repr;
// }
// fae_dynamic_type_repr_t double_get_type(fae_ptr_t a)
// {
//     return f64_type_repr;
// }




// --------------------------------------------------------------------------------
// Wrapper functions
// --------------------------------------------------------------------------------

bool fae_to_bool(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x7 && "Wrong type, expected bool");
    return (p & ~0x7) >> 24;
}

fae_ptr_t fae_from_bool(bool a)
{
    return (fae_ptr_t)(a << 24 & ~0x7 | 0x7);
}

// --------------------------------------------------------------------------------

int8_t fae_to_int8(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x6 && "Wrong type, expected int8");
    return (p & ~0x7) >> 24;
}

fae_ptr_t fae_from_int8(int8_t a)
{
    return (fae_ptr_t)(a << 24 & ~0x7 | 0x6);
}

// --------------------------------------------------------------------------------

int16_t fae_to_int16(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x5 && "Wrong type, expected int16");
    return (p & ~0x7) >> 8;
}

fae_ptr_t fae_from_int16(int16_t a)
{
    return (fae_ptr_t)(a << 8 & ~0x7 | 0x5);
}

// --------------------------------------------------------------------------------

int32_t fae_peek_int32(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x4 && "Wrong type, expected int32");
    int32_t v = *((int32_t *)(p & ~0x7));
    return v;
}

int32_t fae_to_int32(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x4 && "Wrong type, expected int32");
    int32_t v = *((int32_t *)(p & ~0x7));
    free((int32_t *)(p & ~0x7));
    return v;
}

fae_ptr_t fae_copy_int32(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    int32_t *q = malloc(sizeof(int32_t));
    *q = *((int32_t *)(p & ~0x7));
    return (fae_ptr_t)(((intptr_t) q) | 0x4);
}

fae_ptr_t fae_from_int32(int32_t a)
{
    int32_t *p = malloc(sizeof(int32_t));
    *p = a;
    return (fae_ptr_t)(((intptr_t) p) & ~0x7 | 0x4);
}

// --------------------------------------------------------------------------------

int64_t fae_peek_int64(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x3 && "Wrong type, expected int64");
    int64_t v = *((int64_t *)(p & ~0x7));
    return v;
}

int64_t fae_to_int64(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x3 && "Wrong type, expected int64");
    int64_t v = *((int64_t *)(p & ~0x7));
    free((int64_t *)(p & ~0x7));
    return v;
}

fae_ptr_t fae_copy_int64(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    int64_t *q = malloc(sizeof(int64_t));
    *q = *((int64_t *)(p & ~0x7));
    return (fae_ptr_t)(((intptr_t) q) & ~0x7 | 0x3);
}

fae_ptr_t fae_from_int64(int64_t a)
{
    int64_t *p = malloc(sizeof(int64_t));
    *p = a;
    return (fae_ptr_t)(((intptr_t) p) & ~0x7 | 0x3);
}

// --------------------------------------------------------------------------------

float fae_peek_float(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x2 && "Wrong type, expected float");
    float v = *((float *)(p & ~0x7));
    return v;
}
float fae_to_float(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x2 && "Wrong type, expected float");
    float v = *((float *)(p & ~0x7));
    free((float *)(p & ~0x7));
    return v;
}

fae_ptr_t fae_copy_float(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    float *q = malloc(sizeof(float));
    *q = *((float *)(p & ~0x7));
    return (fae_ptr_t)(((intptr_t) q) & ~0x7 | 0x2);
}

fae_ptr_t fae_from_float(float a)
{
    float *p = malloc(sizeof(float));
    *p = a;
    return (fae_ptr_t)(((intptr_t) p) & ~0x7 | 0x2);
}

// --------------------------------------------------------------------------------

double fae_peek_double(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x1 && "Wrong type, expected double");
    double v = *((double *)(p & ~0x7));
    return v;
}
double fae_to_double(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x1 && "Wrong type, expected double");
    double v = *((double *)(p & ~0x7));
    free((double *)(p & ~0x7));
    return v;
}

fae_ptr_t fae_copy_double(fae_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    double *q = malloc(sizeof(double));
    *q = *((double *)(p & ~0x7));
    return (fae_ptr_t)(((intptr_t) q) & ~0x7 | 0x1);
}

fae_ptr_t fae_from_double(double a)
{
    double *p = malloc(sizeof(double));
    *p = a;
    return (fae_ptr_t)(((intptr_t) p) & ~0x7 | 0x1);
}

// --------------------------------------------------------------------------------
// Generic functions
// --------------------------------------------------------------------------------

#define GENERIC1(I,F,A,B) \
    B fae_##F(A a)                                                                      \
    {                                                                                       \
        assert(fae_interface(fae_##I##_i, a) && "Must implement " #I);              \
        return ((fae_##I##_t*) fae_interface(fae_##I##_i, a))->F(a);            \
    }

#define GENERIC2(I,F,A,B,C) \
    C fae_##F(A a, B b)                                                                 \
    {                                                                                       \
        assert(fae_interface(fae_##I##_i, a) && "Must implement " #I);              \
        return ((fae_##I##_t*) fae_interface(fae_##I##_i, a))->F(a, b);         \
    }


GENERIC2(equal,     equal,          fae_ptr_t, fae_ptr_t, bool);
GENERIC2(order,     less_than,      fae_ptr_t, fae_ptr_t, bool);
GENERIC2(order,     greater_than,   fae_ptr_t, fae_ptr_t, bool);

bool fae_not_equal(fae_ptr_t a, fae_ptr_t b)
{
    return !fae_equal(a, b);
}

bool fae_less_than_equal(fae_ptr_t a, fae_ptr_t b)
{
    return fae_less_than(a, b) || fae_equal(a, b);
}

bool fae_greater_than_equal(fae_ptr_t a, fae_ptr_t b)
{
    return fae_greater_than(a, b) || fae_equal(a, b);
}

fae_ptr_t fae_min(fae_ptr_t a, fae_ptr_t b)
{
    return fae_less_than(a, b) ? a : b;
}

fae_ptr_t fae_max(fae_ptr_t a, fae_ptr_t b)
{
    return fae_greater_than(a, b) ? a : b;
}

GENERIC2(number,    add,            fae_ptr_t, fae_ptr_t, fae_ptr_t);
GENERIC2(number,    subtract,       fae_ptr_t, fae_ptr_t, fae_ptr_t);
GENERIC2(number,    multiply,       fae_ptr_t, fae_ptr_t, fae_ptr_t);
GENERIC2(number,    divide,         fae_ptr_t, fae_ptr_t, fae_ptr_t);
GENERIC1(number,    absolute,       fae_ptr_t, fae_ptr_t);

fae_ptr_t fae_dadd(fae_ptr_t a, fae_ptr_t b)
{
    fae_ptr_t res = fae_add(a, b);
    fae_destroy(a);
    fae_destroy(b);
    return res;
}


GENERIC1(copy,      copy,           fae_ptr_t, fae_ptr_t);
GENERIC1(destroy,   destroy,        fae_ptr_t, void);

fae_string_t fae_show(fae_ptr_t a)
{
    return fae_string_show(a);
}

void fae_print(char *f, fae_ptr_t a)
{
    if (a) {
        fae_string_t str = fae_string_show(a);
        char *cstr = fae_string_to_utf8(str);
        printf(f, cstr);
        free(cstr);
        fae_destroy(str);
    } else {
        printf("%s", f);
    }
}

void fae_puts(fae_string_t string)
{
    char *cstr = fae_string_to_utf8(string);
    puts(cstr);
    free(cstr);
}

void fae_dprint(char *f, fae_ptr_t a)
{
    fae_print(f, a);
    fae_destroy(a);
}

void fae_print_ln(fae_ptr_t a)
{
    fae_print("%s\n", a);
}

void fae_dprint_ln(fae_ptr_t a)
{
    fae_dprint("%s\n", a);
}

fae_ptr_t fae_move(fae_ptr_t a)
{
    return a;
}

bool fae_check(fae_ptr_t a)
{
    bool fae_error_check(fae_ptr_t a);
    return fae_error_check(a);
}

// --------------------------------------------------------------------------------
// Value reference ("wrapper") implementations
// --------------------------------------------------------------------------------

#define bool_type_repr_impl bool_type_repr
#define int8_type_repr_impl i8_type_repr
#define int16_type_repr_impl i16_type_repr
#define int32_type_repr_impl i32_type_repr
#define int64_type_repr_impl i64_type_repr
#define float_type_repr_impl f32_type_repr
#define double_type_repr_impl f64_type_repr


#define UNBOXED_WRAPPER_IMPL(T) \
    bool T##_equal(fae_ptr_t a, fae_ptr_t b)                                        \
    {                                                                                       \
        return (fae_to_##T(a) == fae_to_##T(b));                                    \
    }                                                                                       \
    bool T##_less_than(fae_ptr_t a, fae_ptr_t b)                                    \
    {                                                                                       \
        return (fae_to_##T(a) < fae_to_##T(b));                                     \
    }                                                                                       \
    bool T##_greater_than(fae_ptr_t a, fae_ptr_t b)                                 \
    {                                                                                       \
        return (fae_to_##T(a) > fae_to_##T(b));                                     \
    }                                                                                       \
    fae_ptr_t T##_add(fae_ptr_t a, fae_ptr_t b)                                 \
    {                                                                                       \
        return fae_from_##T(fae_to_##T(a) + fae_to_##T(b));                     \
    }                                                                                       \
    fae_ptr_t T##_subtract(fae_ptr_t a, fae_ptr_t b)                            \
    {                                                                                       \
        return fae_from_##T(fae_to_##T(a) - fae_to_##T(b));                     \
    }                                                                                       \
    fae_ptr_t T##_multiply(fae_ptr_t a, fae_ptr_t b)                            \
    {                                                                                       \
        return fae_from_##T(fae_to_##T(a) * fae_to_##T(b));                     \
    }                                                                                       \
    fae_ptr_t T##_divide(fae_ptr_t a, fae_ptr_t b)                              \
    {                                                                                       \
        return fae_from_##T(fae_to_##T(a) / fae_to_##T(b));                     \
    }                                                                                       \
    fae_ptr_t T##_absolute(fae_ptr_t a)                                             \
    {                                                                                       \
        return fae_from_##T(abs(fae_to_##T(a)));                                    \
    }                                                                                       \
    fae_ptr_t T##_copy(fae_ptr_t a)                                                 \
    {                                                                                       \
        return a;                                                                           \
    }                                                                                       \
    fae_dynamic_type_repr_t T##_get_type(fae_ptr_t a)                               \
    {                                                                                       \
        return T##_type_repr_impl;                                                          \
    }                                                                                       \
    void T##_destroy(fae_ptr_t a)                                                       \
    {                                                                                       \
        /* nothing to do */                                                                 \
    }

#define BOXED_WRAPPER_IMPL(T) \
    bool T##_equal(fae_ptr_t a, fae_ptr_t b)                                        \
    {                                                                                       \
        return (fae_peek_##T(a) == fae_peek_##T(b));                                \
    }                                                                                       \
    bool T##_less_than(fae_ptr_t a, fae_ptr_t b)                                    \
    {                                                                                       \
        return (fae_peek_##T(a) < fae_peek_##T(b));                                 \
    }                                                                                       \
    bool T##_greater_than(fae_ptr_t a, fae_ptr_t b)                                 \
    {                                                                                       \
        return (fae_peek_##T(a) > fae_peek_##T(b));                                 \
    }                                                                                       \
    fae_ptr_t T##_add(fae_ptr_t a, fae_ptr_t b)                                 \
    {                                                                                       \
        return fae_from_##T(fae_peek_##T(a) + fae_peek_##T(b));                 \
    }                                                                                       \
    fae_ptr_t T##_subtract(fae_ptr_t a, fae_ptr_t b)                            \
    {                                                                                       \
        return fae_from_##T(fae_peek_##T(a) - fae_peek_##T(b));                 \
    }                                                                                       \
    fae_ptr_t T##_multiply(fae_ptr_t a, fae_ptr_t b)                            \
    {                                                                                       \
        return fae_from_##T(fae_peek_##T(a) * fae_peek_##T(b));                 \
    }                                                                                       \
    fae_ptr_t T##_divide(fae_ptr_t a, fae_ptr_t b)                              \
    {                                                                                       \
        return fae_from_##T(fae_peek_##T(a) / fae_peek_##T(b));                 \
    }                                                                                       \
    fae_ptr_t T##_absolute(fae_ptr_t a)                                             \
    {                                                                                       \
        return fae_from_##T(abs(fae_peek_##T(a))); /* TODO use tg? */               \
    }                                                                                       \
    fae_ptr_t T##_copy(fae_ptr_t a)                                                 \
    {                                                                                       \
        return fae_copy_##T(a);                                                         \
    }                                                                                       \
    fae_dynamic_type_repr_t T##_get_type(fae_ptr_t a)                               \
    {                                                                                       \
        return T##_type_repr_impl;                                                          \
    }                                                                                       \
    void T##_destroy(fae_ptr_t a)                                                       \
    {                                                                                       \
        fae_to_##T(a);                                                                  \
    }

#define UNBOXED_SHOW_IMPL(T,F) \
    fae_string_t T##_show(fae_ptr_t a)                                              \
    {                                                                                       \
        int  n;                                                                             \
        char cs[16];                                                                        \
        n = snprintf(cs, 16, F, fae_to_##T(a));                                         \
        cs[n] = 0; /* terminate */                                                          \
        return fae_string_from_utf8(cs);                                                \
    }

#define BOXED_SHOW_IMPL(T,F) \
    fae_string_t T##_show(fae_ptr_t a)                                              \
    {                                                                                       \
        int  n;                                                                             \
        char cs[16];                                                                        \
        n = snprintf(cs, 16, F, fae_peek_##T(a));                                       \
        cs[n] = 0; /* terminate */                                                          \
        return fae_string_from_utf8(cs);                                                \
    }

/* Generates T_impl
 */
#define IMPLEMENT_WRAPPER(T) \
    fae_ptr_t T##_impl(fae_id_t interface)                                          \
    {                                                                                       \
        static fae_equal_t   T##_equal_impl   =                                         \
            { T##_equal };                                                                  \
        static fae_order_t   T##_order_impl   =                                         \
            { T##_less_than, T##_greater_than };                                            \
        static fae_number_t  T##_number_impl  =                                         \
            { T##_add, T##_subtract, T##_multiply, T##_divide, T##_absolute };              \
        static fae_string_show_t    T##_show_impl    =                                  \
            { T##_show };                                                                   \
        static fae_copy_t    T##_copy_impl    =                                         \
            { T##_copy };                                                                   \
        static fae_dynamic_t T##_dynamic_impl =                                         \
            { T##_get_type };                                                               \
        static fae_destroy_t T##_destroy_impl =                                         \
            { T##_destroy };                                                                \
                                                                                            \
        switch (interface)                                                                  \
        {                                                                                   \
        case fae_equal_i:                                                               \
            return &T##_equal_impl;                                                         \
        case fae_order_i:                                                               \
            return &T##_order_impl;                                                         \
        case fae_number_i:                                                              \
            return &T##_number_impl;                                                        \
        case fae_string_show_i:                                                         \
            return &T##_show_impl;                                                          \
        case fae_copy_i:                                                                \
            return &T##_copy_impl;                                                          \
        case fae_dynamic_i:                                                             \
            return &T##_dynamic_impl;                                                       \
        case fae_destroy_i:                                                             \
            return &T##_destroy_impl;                                                       \
        default:                                                                            \
            return NULL;                                                                    \
        }                                                                                   \
    }


UNBOXED_WRAPPER_IMPL(bool);
UNBOXED_WRAPPER_IMPL(int8);
UNBOXED_WRAPPER_IMPL(int16);
BOXED_WRAPPER_IMPL(int32);
BOXED_WRAPPER_IMPL(int64);
BOXED_WRAPPER_IMPL(float);
BOXED_WRAPPER_IMPL(double);

fae_string_t bool_show(fae_ptr_t a)
{
    return fae_to_bool(a) ? fae_string_from_utf8("true")
           : fae_string_from_utf8("false");
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
struct fae_impl_disp {
    fae_impl_t impl;
};
// @endcond

fae_ptr_t fae_interface(fae_id_t type, fae_ptr_t pointer)
{
    assert(pointer && "The null pointer have no interfaces");

    switch (fae_type(pointer)) {
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
        return ((struct fae_impl_disp *) pointer)->impl(type);
    }
}
