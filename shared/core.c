
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa.h>
#include <fa/dynamic.h>
#include <fa/string.h>

int fa_type(fa_ptr_t a)
{
    return ((intptr_t) a) & 0x7;
}

char *fa_type_str(fa_ptr_t a)
{
    switch (fa_type(a)) {
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

bool fa_is_bool(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x7;
}
bool fa_is_int8(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x6;
}
bool fa_is_int16(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x5;
}
bool fa_is_int32(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x4;
}
bool fa_is_int64(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x3;
}
bool fa_is_float(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x2;
}
bool fa_is_double(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x1;
}
bool fa_is_ref(fa_ptr_t x)
{
    return (((intptr_t) x) & 0x7) == 0x0;
}

// fa_dynamic_type_repr_t bool_get_type(fa_ptr_t a)
// {
//     bool_type_repr;
// }
// fa_dynamic_type_repr_t int8_get_type(fa_ptr_t a)
// {
//     return i8_type_repr;
// }
// fa_dynamic_type_repr_t int16_get_type(fa_ptr_t a)
// {
//     return i16_type_repr;
// }
// fa_dynamic_type_repr_t int32_get_type(fa_ptr_t a)
// {
//     return i32_type_repr;
// }
// fa_dynamic_type_repr_t int64_get_type(fa_ptr_t a)
// {
//     return i64_type_repr;
// }
// fa_dynamic_type_repr_t float_get_type(fa_ptr_t a)
// {
//     return f32_type_repr;
// }
// fa_dynamic_type_repr_t double_get_type(fa_ptr_t a)
// {
//     return f64_type_repr;
// }




// --------------------------------------------------------------------------------
// Wrapper functions
// --------------------------------------------------------------------------------

bool fa_to_bool(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x7 && "Wrong type, expected bool");
    return (p & ~0x7) >> 24;
}

fa_ptr_t fa_from_bool(bool a)
{
    return (fa_ptr_t)(((intptr_t) a << 24 & ~0x7) | 0x7);
}

// --------------------------------------------------------------------------------

int8_t fa_to_int8(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x6 && "Wrong type, expected int8");
    return (p & ~0x7) >> 24;
}

fa_ptr_t fa_from_int8(int8_t a)
{
    return (fa_ptr_t)(((intptr_t) a << 24 & ~0x7) | 0x6);
}

// --------------------------------------------------------------------------------

int16_t fa_to_int16(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x5 && "Wrong type, expected int16");
    return (p & ~0x7) >> 8;
}

fa_ptr_t fa_from_int16(int16_t a)
{
    return (fa_ptr_t)(((intptr_t) a << 8 & ~0x7) | 0x5);
}

// --------------------------------------------------------------------------------

int32_t fa_peek_int32(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x4 && "Wrong type, expected int32");
    int32_t v = *((int32_t *)(p & ~0x7));
    return v;
}

int32_t fa_to_int32(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x4 && "Wrong type, expected int32");
    int32_t v = *((int32_t *)(p & ~0x7));
    free((int32_t *)(p & ~0x7));
    return v;
}

fa_ptr_t fa_copy_int32(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    int32_t *q = malloc(sizeof(int32_t));
    *q = *((int32_t *)(p & ~0x7));
    return (fa_ptr_t)(((intptr_t) q) | 0x4);
}

fa_ptr_t fa_from_int32(int32_t a)
{
    int32_t *p = malloc(sizeof(int32_t));
    *p = a;
    return (fa_ptr_t)((((intptr_t) p) & ~0x7) | 0x4);
}

// --------------------------------------------------------------------------------

int64_t fa_peek_int64(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x3 && "Wrong type, expected int64");
    int64_t v = *((int64_t *)(p & ~0x7));
    return v;
}

int64_t fa_to_int64(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x3 && "Wrong type, expected int64");
    int64_t v = *((int64_t *)(p & ~0x7));
    free((int64_t *)(p & ~0x7));
    return v;
}

fa_ptr_t fa_copy_int64(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    int64_t *q = malloc(sizeof(int64_t));
    *q = *((int64_t *)(p & ~0x7));
    return (fa_ptr_t)((((intptr_t) q) & ~0x7) | 0x3);
}

fa_ptr_t fa_from_int64(int64_t a)
{
    int64_t *p = malloc(sizeof(int64_t));
    *p = a;
    return (fa_ptr_t)((((intptr_t) p) & ~0x7) | 0x3);
}

// --------------------------------------------------------------------------------

float fa_peek_float(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x2 && "Wrong type, expected float");
    float v = *((float *)(p & ~0x7));
    return v;
}
float fa_to_float(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x2 && "Wrong type, expected float");
    float v = *((float *)(p & ~0x7));
    free((float *)(p & ~0x7));
    return v;
}

fa_ptr_t fa_copy_float(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    float *q = malloc(sizeof(float));
    *q = *((float *)(p & ~0x7));
    return (fa_ptr_t)((((intptr_t) q) & ~0x7) | 0x2);
}

fa_ptr_t fa_from_float(float a)
{
    float *p = malloc(sizeof(float));
    *p = a;
    return (fa_ptr_t)((((intptr_t) p) & ~0x7) | 0x2);
}

// --------------------------------------------------------------------------------

double fa_peek_double(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x1 && "Wrong type, expected double");
    double v = *((double *)(p & ~0x7));
    return v;
}
double fa_to_double(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    assert((p & 0x7) == 0x1 && "Wrong type, expected double");
    double v = *((double *)(p & ~0x7));
    free((double *)(p & ~0x7));
    return v;
}

fa_ptr_t fa_copy_double(fa_ptr_t a)
{
    intptr_t p = (intptr_t) a;
    double *q = malloc(sizeof(double));
    *q = *((double *)(p & ~0x7));
    return (fa_ptr_t)((((intptr_t) q) & ~0x7) | 0x1);
}

fa_ptr_t fa_from_double(double a)
{
    double *p = malloc(sizeof(double));
    *p = a;
    return (fa_ptr_t)((((intptr_t) p) & ~0x7) | 0x1);
}

// --------------------------------------------------------------------------------

#define GENERIC1(I,F,A,B) \
    B fa_##F(A a)                                                      \
    {                                                                   \
        assert(fa_interface(fa_##I##_i, a) && "Must implement " #I);  \
        return ((fa_##I##_t*) fa_interface(fa_##I##_i, a))->F(a);    \
    }

#define GENERIC2(I,F,A,B,C) \
    C fa_##F(A a, B b)                                                 \
    {                                                                   \
        assert(fa_interface(fa_##I##_i, a) && "Must implement " #I);  \
        return ((fa_##I##_t*) fa_interface(fa_##I##_i, a))->F(a, b); \
    }


GENERIC2(equal,     equal,          fa_ptr_t, fa_ptr_t, bool);
GENERIC2(order,     less_than,      fa_ptr_t, fa_ptr_t, bool);
GENERIC2(order,     greater_than,   fa_ptr_t, fa_ptr_t, bool);

bool fa_not_equal(fa_ptr_t a, fa_ptr_t b)
{
    return !fa_equal(a, b);
}

bool fa_less_than_equal(fa_ptr_t a, fa_ptr_t b)
{
    return fa_less_than(a, b) || fa_equal(a, b);
}

bool fa_greater_than_equal(fa_ptr_t a, fa_ptr_t b)
{
    return fa_greater_than(a, b) || fa_equal(a, b);
}

fa_ptr_t fa_min(fa_ptr_t a, fa_ptr_t b)
{
    return fa_less_than(a, b) ? a : b;
}

fa_ptr_t fa_max(fa_ptr_t a, fa_ptr_t b)
{
    return fa_greater_than(a, b) ? a : b;
}

GENERIC2(number,    add,            fa_ptr_t, fa_ptr_t, fa_ptr_t);
GENERIC2(number,    subtract,       fa_ptr_t, fa_ptr_t, fa_ptr_t);
GENERIC2(number,    multiply,       fa_ptr_t, fa_ptr_t, fa_ptr_t);
GENERIC2(number,    divide,         fa_ptr_t, fa_ptr_t, fa_ptr_t);
GENERIC1(number,    absolute,       fa_ptr_t, fa_ptr_t);

fa_ptr_t fa_dadd(fa_ptr_t a, fa_ptr_t b)
{
    fa_ptr_t res = fa_add(a, b);
    fa_destroy(a);
    fa_destroy(b);
    return res;
}


GENERIC1(copy,      copy,           fa_ptr_t, fa_ptr_t);
GENERIC1(destroy,   destroy,        fa_ptr_t, void);

fa_string_t fa_show(fa_ptr_t a)
{
    return fa_string_show(a);
}

fa_string_t fa_to_string(fa_ptr_t a)
{
    return fa_string_to_string(a);
}


void fa_print(char *f, fa_ptr_t a)
{
    if (a) {
        fa_string_t str = fa_string_to_string(a);
        char *cstr = fa_string_to_utf8(str);
        printf(f, cstr);
        free(cstr);
        fa_destroy(str);
    } else {
        printf("%s", f);
    }
}

void fa_puts(fa_string_t string)
{
    char *cstr = fa_string_to_utf8(string);
    puts(cstr);
    free(cstr);
}

void fa_dprint(char *f, fa_ptr_t a)
{
    fa_print(f, a);
    fa_destroy(a);
}

void fa_print_ln(fa_ptr_t a)
{
    if (a) {
        fa_print("%s\n", a);
    } else {
        fa_print("", a);
    }
}

void fa_dprint_ln(fa_ptr_t a)
{
    fa_dprint("%s\n", a);
}

fa_ptr_t fa_move(fa_ptr_t a)
{
    return a;
}

bool fa_check(fa_ptr_t a)
{
    bool fa_error_check(fa_ptr_t a);
    return fa_error_check(a);
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
    bool T##_equal(fa_ptr_t a, fa_ptr_t b)                                \
    {                                                                       \
        return (fa_to_##T(a) == fa_to_##T(b));                            \
    }                                                                       \
    bool T##_less_than(fa_ptr_t a, fa_ptr_t b)                            \
    {                                                                       \
        return (fa_to_##T(a) < fa_to_##T(b));                             \
    }                                                                       \
    bool T##_greater_than(fa_ptr_t a, fa_ptr_t b)                         \
    {                                                                       \
        return (fa_to_##T(a) > fa_to_##T(b));                             \
    }                                                                       \
    fa_ptr_t T##_add(fa_ptr_t a, fa_ptr_t b)                             \
    {                                                                       \
        return fa_from_##T(fa_to_##T(a) + fa_to_##T(b));                 \
    }                                                                       \
    fa_ptr_t T##_subtract(fa_ptr_t a, fa_ptr_t b)                        \
    {                                                                       \
        return fa_from_##T(fa_to_##T(a) - fa_to_##T(b));                 \
    }                                                                       \
    fa_ptr_t T##_multiply(fa_ptr_t a, fa_ptr_t b)                        \
    {                                                                       \
        return fa_from_##T(fa_to_##T(a) * fa_to_##T(b));                 \
    }                                                                       \
    fa_ptr_t T##_divide(fa_ptr_t a, fa_ptr_t b)                          \
    {                                                                       \
        return fa_from_##T(fa_to_##T(a) / fa_to_##T(b));                 \
    }                                                                       \
    fa_ptr_t T##_absolute(fa_ptr_t a)                                     \
    {                                                                       \
        return fa_from_##T(abs(fa_to_##T(a)));                            \
    }                                                                       \
    fa_ptr_t T##_copy(fa_ptr_t a)                                         \
    {                                                                       \
        return a;                                                           \
    }                                                                       \
    fa_dynamic_type_repr_t T##_get_type(fa_ptr_t a)                       \
    {                                                                       \
        return T##_type_repr_impl;                                          \
    }                                                                       \
    void T##_destroy(fa_ptr_t a)                                           \
    {                                                                       \
        /* nothing to do */                                                 \
    }

#define BOXED_WRAPPER_IMPL(T) \
    bool T##_equal(fa_ptr_t a, fa_ptr_t b)                                \
    {                                                                       \
        return (fa_peek_##T(a) == fa_peek_##T(b));                        \
    }                                                                       \
    bool T##_less_than(fa_ptr_t a, fa_ptr_t b)                            \
    {                                                                       \
        return (fa_peek_##T(a) < fa_peek_##T(b));                         \
    }                                                                       \
    bool T##_greater_than(fa_ptr_t a, fa_ptr_t b)                         \
    {                                                                       \
        return (fa_peek_##T(a) > fa_peek_##T(b));                         \
    }                                                                       \
    fa_ptr_t T##_add(fa_ptr_t a, fa_ptr_t b)                             \
    {                                                                       \
        return fa_from_##T(fa_peek_##T(a) + fa_peek_##T(b));             \
    }                                                                       \
    fa_ptr_t T##_subtract(fa_ptr_t a, fa_ptr_t b)                        \
    {                                                                       \
        return fa_from_##T(fa_peek_##T(a) - fa_peek_##T(b));             \
    }                                                                       \
    fa_ptr_t T##_multiply(fa_ptr_t a, fa_ptr_t b)                        \
    {                                                                       \
        return fa_from_##T(fa_peek_##T(a) * fa_peek_##T(b));             \
    }                                                                       \
    fa_ptr_t T##_divide(fa_ptr_t a, fa_ptr_t b)                          \
    {                                                                       \
        return fa_from_##T(fa_peek_##T(a) / fa_peek_##T(b));             \
    }                                                                       \
    fa_ptr_t T##_absolute(fa_ptr_t a)                                     \
    {                                                                       \
        return fa_from_##T(abs(fa_peek_##T(a))); /* TODO use tg? */       \
    }                                                                       \
    fa_ptr_t T##_copy(fa_ptr_t a)                                         \
    {                                                                       \
        return fa_copy_##T(a);                                             \
    }                                                                       \
    fa_dynamic_type_repr_t T##_get_type(fa_ptr_t a)                       \
    {                                                                       \
        return T##_type_repr_impl;                                          \
    }                                                                       \
    void T##_destroy(fa_ptr_t a)                                           \
    {                                                                       \
        fa_to_##T(a);                                                      \
    }

#define UNBOXED_SHOW_IMPL(T,F) \
    fa_string_t T##_show(fa_ptr_t a)                                      \
    {                                                                       \
        int  n;                                                             \
        char cs[16];                                                        \
        n = snprintf(cs, 16, F, fa_to_##T(a));                             \
        cs[n] = 0; /* terminate */                                          \
        return fa_string_from_utf8(cs);                                    \
    }

#define BOXED_SHOW_IMPL(T,F) \
    fa_string_t T##_show(fa_ptr_t a)                                      \
    {                                                                       \
        int  n;                                                             \
        char cs[16];                                                        \
        n = snprintf(cs, 16, F, fa_peek_##T(a));                           \
        cs[n] = 0; /* terminate */                                          \
        return fa_string_from_utf8(cs);                                    \
    }

/* Generates T_impl
 */
#define IMPLEMENT_WRAPPER(T) \
    fa_ptr_t T##_impl(fa_id_t interface)                                          \
    {                                                                               \
        static fa_equal_t   T##_equal_impl   =                                     \
            { T##_equal };                                                          \
        static fa_order_t   T##_order_impl   =                                     \
            { T##_less_than, T##_greater_than };                                    \
        static fa_number_t  T##_number_impl  =                                     \
            { T##_add, T##_subtract, T##_multiply, T##_divide, T##_absolute };      \
        static fa_string_show_t    T##_show_impl    =                              \
            { T##_show };                                                           \
        static fa_copy_t    T##_copy_impl    =                                     \
            { T##_copy };                                                           \
        static fa_dynamic_t T##_dynamic_impl =                                     \
            { T##_get_type };                                                       \
        static fa_destroy_t T##_destroy_impl =                                     \
            { T##_destroy };                                                        \
                                                                                    \
        switch (interface)                                                          \
        {                                                                           \
        case fa_equal_i:                                                           \
            return &T##_equal_impl;                                                 \
        case fa_order_i:                                                           \
            return &T##_order_impl;                                                 \
        case fa_number_i:                                                          \
            return &T##_number_impl;                                                \
        case fa_string_show_i:                                                     \
            return &T##_show_impl;                                                  \
        case fa_copy_i:                                                            \
            return &T##_copy_impl;                                                  \
        case fa_dynamic_i:                                                         \
            return &T##_dynamic_impl;                                               \
        case fa_destroy_i:                                                         \
            return &T##_destroy_impl;                                               \
        default:                                                                    \
            return NULL;                                                            \
        }                                                                           \
    }


UNBOXED_WRAPPER_IMPL(bool);
UNBOXED_WRAPPER_IMPL(int8);
UNBOXED_WRAPPER_IMPL(int16);
BOXED_WRAPPER_IMPL(int32);
BOXED_WRAPPER_IMPL(int64);
BOXED_WRAPPER_IMPL(float);
BOXED_WRAPPER_IMPL(double);

fa_string_t bool_show(fa_ptr_t a)
{
    return fa_to_bool(a) ? fa_string_from_utf8("true")
           : fa_string_from_utf8("false");
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



fa_ptr_t fa_interface(fa_id_t type, fa_ptr_t pointer)
{
    assert(pointer && "The null pointer have no interfaces");
    struct fa_impl_disp { fa_impl_t impl; };

    switch (fa_type(pointer)) {
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
        return ((struct fa_impl_disp *) pointer)->impl(type);
    }
}
