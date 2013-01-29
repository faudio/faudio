
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/string.h>
#import <doremir/util.h>
#import "vm.h"

#define lmm_malloc  malloc
#define lmm_calloc  calloc
#define lmm_realloc realloc
#define lmm_free    free

#define lmm_for_each_register(var, num, lmm) \
  for (size_t num = 0; num < kLmmRegs; ++num) \
    doremir_let (var, lmm->regs[num])

lmm_t lmm_create()
{
    lmm_t vm = lmm_malloc(sizeof(struct lmm));
    memset(vm, 0, sizeof(struct lmm));
    return vm;
}

void lmm_destroy(lmm_t lmm)
{
    lmm_for_each_register(reg, count, lmm) {
        lmm_free(reg.data);
    }

    lmm_free(lmm->error);
    lmm_free(lmm);
}

char * lmm_get_error(lmm_t lmm)
{
    return lmm->error;
}

#define LMM_SHOW(N,T,F,M)                                                         \
  string_t lmm_show_##N(lmm_t lmm)                                                \
  {                                                                               \
    string_t str = string("\n");                                                  \
                                                                                  \
    lmm_for_each_register(reg, id, lmm) {                                         \
                                                                                  \
      /* Print all non-empty registers */                                         \
                                                                                  \
      if (reg.size) {                                                             \
        str = string_dappend(str, format_int("\nr%d:\t", id));                    \
                                                                                  \
        for (size_t i = 0; i < (reg.size / sizeof(T)); ++i) {                     \
          str = string_dappend(str, string(" "));                                 \
          str = string_dappend(str,                                               \
                               M(F, ((T*) reg.data)[i]));                         \
        }                                                                         \
                                                                                  \
        str = string_dappend(str, string("\n"));                                  \
      }                                                                           \
    }                                                                             \
    return str;                                                                   \
  }                                                                               \
 
LMM_SHOW(i8,  uint8_t,   "%02x",  format_integer);
LMM_SHOW(i16, uint16_t,  "%04x",  format_integer);
LMM_SHOW(i32, uint32_t,  "%08x",  format_integer);
LMM_SHOW(i64, uint64_t,  "%016x", format_integer);
LMM_SHOW(f32, float,     "%0lf",  format_floating);
LMM_SHOW(f64, double,    "%0lf",  format_floating);
// LMM_SHOW(ptr, ptr_t,     "%02p");


size_t lmm_get_reg_size(lmm_t lmm, lmm_reg_t r)
{
    return lmm->regs[r].size;
}

size_t lmm_get_reg_max_size(lmm_t lmm, lmm_reg_t r)
{
    return lmm->regs[r].maxSize;
}

void * lmm_get_reg_data(lmm_t lmm, lmm_reg_t r)
{
    return lmm->regs[r].data;
}


// Memory

#define rmax(r)  lmm->regs[r].maxSize
#define rsize(r) lmm->regs[r].size
#define rdata(r) lmm->regs[r].data

void lmm_alloc(lmm_t lmm, size_t size, lmm_reg_t r)
{
    rdata(r) = lmm_realloc(rdata(r), size);
    rsize(r) = size;
    rmax(r)  = size;

    memset(rdata(r), 0, size);
}

// swap r1 r2
// (r1,r2) <==> (r2,r1)
void lmm_swap(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2)
{
    size_t ts = rsize(r2);
    size_t tm = rmax(r2);
    void * td = rdata(r2);

    rsize(r2) = rsize(r1);
    rmax(r2) = rmax(r1);
    rdata(r2) = rdata(r1);

    rsize(r1) = ts;
    rmax(r1) = tm;
    rdata(r1) = td;
}

// copy r1 r2
// r1 ==> r2
void lmm_copy(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2)
{
    assert(rmax(r2) >= rsize(r1)            && "Can not copy: second operand is too small");

    memcpy(rdata(r2), rdata(r1), rsize(r1));
    rsize(r2) = rsize(r1);
}

void lmm_split(lmm_t lmm, size_t split, lmm_reg_t r1, lmm_reg_t r2)
{
    assert(rsize(r1)        >= split        && "Can not split: too large size");
    assert(rmax(r2) + split >= rsize(r1)    && "Can not split: second operand too small");

    int len = rsize(r1) - split;
    memcpy(rdata(r2), rdata(r1) + split, len);
    rsize(r1) = split;
    rsize(r2) = len;
}


// Set and Ap

#define LLM_SET(N, T)                                                                       \
  void lmm_set_##N(lmm_t lmm, T x, lmm_reg_t r1)                                            \
  {                                                                                         \
    assert (rsize(r1) > 0 && "Can not set: empty register.");                               \
    typedef T data_t;                                                                       \
                                                                                            \
    size_t  count = rsize(r1) / sizeof(data_t);                                             \
    data_t  *data = (data_t *) rdata(r1);                                                   \
                                                                                            \
    for (size_t i = 0; i < count; ++i)                                                      \
      data[i] = x;                                                                          \
  }                                                                                         \
 
#define LLM_AP1(N1, N2, T1, T2)                                                             \
  void lmm_ap1_##N1##_##N2(                                                                 \
    lmm_t lmm,                                                                              \
    unary_t f,                                                                              \
    ptr_t ct,                                                                               \
    lmm_reg_t r1                                                                            \
  )                                                                                         \
  {                                                                                         \
    typedef T1 arg_t;                                                                       \
    typedef T2 res_t;                                                                       \
    typedef res_t (*func_t) (ptr_t, arg_t);                                                 \
                                                                                            \
    arg_t  *data = (arg_t *) rdata(r1);                                                     \
    func_t  func = (func_t) f;                                                              \
                                                                                            \
    size_t   count = rsize(r1) / sizeof(arg_t);                                             \
                                                                                            \
    for (size_t i = 0; i < count; ++i) {                                                    \
      data[i] = func(ct, data[i]);                                                          \
    }                                                                                       \
  }                                                                                         \
 

#define LLM_AP2(N1, N2, N3, T1, T2, T3)                                                     \
  void lmm_ap2_##N1##_##N2##_##N3(                                                          \
    lmm_t lmm,                                                                              \
    binary_t f,                                                                             \
    ptr_t ct,                                                                               \
    lmm_reg_t r1,                                                                           \
    lmm_reg_t r2                                                                            \
  )                                                                                         \
  {                                                                                         \
    typedef T1 arg1_t;                                                                      \
    typedef T2 arg2_t;                                                                      \
    typedef T3 res_t;                                                                       \
    typedef res_t (*func_t) (ptr_t, arg1_t, arg2_t);                                        \
                                                                                            \
    arg1_t *data1 = (arg1_t *) rdata(r1);                                                   \
    arg2_t *data2 = (arg2_t *) rdata(r2);                                                   \
    func_t func = (func_t) f;                                                               \
                                                                                            \
    size_t  count = size_min(rsize(r1) / sizeof(arg1_t), rsize(r2) / sizeof(arg2_t));       \
                                                                                            \
    for (size_t i = 0; i < count; ++i) {                                                    \
      data1[i] = func(ct, data1[i], data2[i]);                                              \
    }                                                                                       \
  }                                                                                         \
 

#define LLM_NV_OP(NOP, OP, N1, N2, T1, T2)                                                  \
  void lmm_##NOP##_##N1##_##N2(                                                             \
    lmm_t lmm,                                                                              \
    lmm_reg_t r1,                                                                           \
    lmm_reg_t r2                                                                            \
  )                                                                                         \
  {                                                                                         \
    typedef T1 arg1_t;                                                                      \
    typedef T2 arg2_t;                                                                      \
                                                                                            \
    arg1_t *data1 = (arg1_t *) rdata(r1);                                                   \
    arg2_t *data2 = (arg2_t *) rdata(r2);                                                   \
                                                                                            \
    size_t  count = size_min(rsize(r1) / sizeof(arg1_t), rsize(r2) / sizeof(arg2_t));       \
                                                                                            \
    for (size_t i = 0; i < count; ++i) {                                                    \
      data1[i] = data1[i] OP data2[i];                                                      \
    }                                                                                       \
  }                                                                                         \
 
// typedef float v256xf32 __attribute__((vector_size(256 * sizeof(float))));
// typedef uint8_t v256xi8 __attribute__((vector_size(256 * sizeof(uint8_t))));

// TODO
// We only know size at runtime
// Implement by splitting into chunks of some fixed size and invoke vops on that
void lmm_vadd_i8_i8(
    lmm_t lmm,
    lmm_reg_t r1,
    lmm_reg_t r2
)
{
    typedef uint8_t arg1_t;
    typedef uint8_t arg2_t;

    arg1_t * data1 = (arg1_t *) rdata(r1);
    arg2_t * data2 = (arg2_t *) rdata(r2);

    size_t  count = size_min(rsize(r1) / sizeof(arg1_t), rsize(r2) / sizeof(arg2_t));

    for (size_t i = 0; i < count; ++i) {
        data1[i] = data1[i] + data2[i];
    }
}


LLM_SET(i8,  uint8_t);
LLM_SET(i16, uint16_t);
LLM_SET(i32, uint32_t);
LLM_SET(i64, uint64_t);
LLM_SET(f32, float);
LLM_SET(f64, double);
LLM_SET(ptr, ptr_t);

LLM_AP1(i8,  i8,  uint8_t, uint8_t);
LLM_AP1(f32, f32, float,   float);

LLM_AP2(i8,  i8,  i8,  uint8_t, uint8_t, uint8_t);
// LLM_AP2(f32, f32, f32, float,   float,   float);



LLM_NV_OP(add , +  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(sub , -  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(mul , *  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(div , /  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(rem , %  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP( and , &  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP( or  , |  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(xor , ^  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(eq  , == , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(ne  , != , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(lt  , <  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(gt  , >  , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(lte , <= , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(gte , >= , i8, i8, uint8_t , uint8_t);
LLM_NV_OP(add , +  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(sub , -  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(mul , *  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(div , /  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(rem , %  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP( and , &  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP( or  , |  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(xor , ^  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(eq  , == , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(ne  , != , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(lt  , <  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(gt  , >  , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(lte , <= , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(gte , >= , i16, i16, uint16_t, uint16_t);
LLM_NV_OP(add , +  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(sub , -  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(mul , *  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(div , /  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(rem , %  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP( and , &  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP( or  , |  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(xor , ^  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(eq  , == , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(ne  , != , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(lt  , <  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(gt  , >  , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(lte , <= , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(gte , >= , i32, i32, uint32_t, uint32_t);
LLM_NV_OP(add , +  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(sub , -  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(mul , *  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(div , /  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(rem , %  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP( and , &  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP( or  , |  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(xor , ^  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(eq  , == , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(ne  , != , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(lt  , <  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(gt  , >  , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(lte , <= , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(gte , >= , i64, i64, uint64_t, uint64_t);
LLM_NV_OP(add , +  , f32, f32, float   , float);
LLM_NV_OP(sub , -  , f32, f32, float   , float);
LLM_NV_OP(mul , *  , f32, f32, float   , float);
LLM_NV_OP(div , /  , f32, f32, float   , float);
// LLM_NV_OP(rem , %  , f32, f32, float   , float   );
// LLM_NV_OP(and , &  , f32, f32, float   , float   );
// LLM_NV_OP(or  , |  , f32, f32, float   , float   );
// LLM_NV_OP(xor , ^  , f32, f32, float   , float   );
LLM_NV_OP(eq  , == , f32, f32, float   , float);
LLM_NV_OP(ne  , != , f32, f32, float   , float);
LLM_NV_OP(lt  , <  , f32, f32, float   , float);
LLM_NV_OP(gt  , >  , f32, f32, float   , float);
LLM_NV_OP(lte , <= , f32, f32, float   , float);
LLM_NV_OP(gte , >= , f32, f32, float   , float);
LLM_NV_OP(add , +  , f64, f64, double  , double);
LLM_NV_OP(sub , -  , f64, f64, double  , double);
LLM_NV_OP(mul , *  , f64, f64, double  , double);
LLM_NV_OP(div , /  , f64, f64, double  , double);
// LLM_NV_OP(rem , %  , f64, f64, double  , double  );
// LLM_NV_OP(and , &  , f64, f64, double  , double  );
// LLM_NV_OP(or  , |  , f64, f64, double  , double  );
// LLM_NV_OP(xor , ^  , f64, f64, double  , double  );
LLM_NV_OP(eq  , == , f64, f64, double  , double);
LLM_NV_OP(ne  , != , f64, f64, double  , double);
LLM_NV_OP(lt  , <  , f64, f64, double  , double);
LLM_NV_OP(gt  , >  , f64, f64, double  , double);
LLM_NV_OP(lte , <= , f64, f64, double  , double);
LLM_NV_OP(gte , >= , f64, f64, double  , double);






// Tests

int8_t my_succ_i8(ptr_t c, int8_t x)
{
    return x + 1;
}
int8_t my_add_i8(ptr_t c, int8_t x, int8_t y)
{
    return x + 1;
}

// FIXME lt, gt etc for floats

void test_vm_loop()
{
    lmm_t vm = lmm_create();

    size_t n = 4;
    lmm_alloc(vm, 8 * n, 0);
    lmm_alloc(vm, 8 * n, 1);
    lmm_alloc(vm, 8 * n, 2);
    lmm_alloc(vm, 8 * n, 3);
    lmm_alloc(vm, 8 * n, 4);

    lmm_set_f64(vm, 3.14f, 0);
    lmm_set_f64(vm, 0.1f,  1);
    ((double *)vm->regs[0].data)[0] = 0.23;
    ((double *)vm->regs[0].data)[3] = 5.0;

    doremir_print_ln(lmm_show_f64(vm));

    // lmm_swap(vm, 0, 1);
    // lmm_swap(vm, 3, 0);
    // lmm_swap(vm, 3, 3);
    lmm_copy(vm, 0, 3);
    lmm_copy(vm, 1, 4);
    // lmm_split(vm, 13, 0, 10);

    // lmm_ap1_i8_i8(vm, (unary_t) my_succ_i8, NULL, 0);
    // lmm_ap1_i8_i8(vm, (unary_t) my_succ_i8, NULL, 1);
    // lmm_ap2_i8_i8_i8(vm, (binary_t) my_add_i8, NULL, 0, 1);

    lmm_add_f64_f64(vm, 3, 4);
    lmm_add_f64_f64(vm, 2, 3);
    lmm_add_f64_f64(vm, 0, 1);
    lmm_lt_f64_f64(vm, 0, 2);


    // printf("%f\n", ((double*)vm->regs[0].data)[n-1]);
    doremir_print_ln(lmm_show_f64(vm));
    lmm_destroy(vm);
}

void test_vm()
{
    // while(1)
    {
        test_vm_loop();
        // doremir_thread_sleep(1000);
    }
}