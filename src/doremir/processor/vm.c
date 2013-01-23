
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
  return lmm_calloc(1, sizeof(struct lmm));
}

void lmm_destroy(lmm_t lmm)
{
  lmm_for_each_register(reg, count, lmm) {
    lmm_free(reg.data);
  }

  lmm_free(lmm->error);
  lmm_free(lmm);
}

char *lmm_get_error(lmm_t lmm)
{
  return lmm->error;
}

string_t lmm_show(lmm_t lmm)
{
  string_t str = string("\n");

  lmm_for_each_register(reg, id, lmm) {

    // Print all non-empty registers
    // Just plain byte output by now

    if (reg.size) {
      str = string_dappend(str, format_int("\nr%d:\t", id));

      for (size_t i = 0; i < reg.size; ++i) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str,
                             format_int("%02x", ((uint8_t *) reg.data)[i]));
      }

      str = string_dappend(str, string("\n"));
    }
  }
  return str;
}

size_t lmm_get_reg_size(lmm_t lmm, lmm_reg_t r)
{
  return lmm->regs[r].size;
}

size_t lmm_get_reg_max_size(lmm_t lmm, lmm_reg_t r)
{
  return lmm->regs[r].maxSize;
}

void *lmm_get_reg_data(lmm_t lmm, lmm_reg_t r)
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

void lmm_swap(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2)
{
  size_t ts = rsize(r2);
  size_t tm = rmax(r2);
  void  *td = rdata(r2);

  rsize(r2) = rsize(r1);
  rmax(r2) = rmax(r1);
  rdata(r2) = rdata(r1);

  rsize(r1) = ts;
  rmax(r1) = tm;
  rdata(r1) = td;
}



void lmm_dup(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2)
{
  assert(rmax(r2) >= rsize(r1)            && "Can not dup: second operand is too small");

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

LLM_NV_OP(add, +, i8, i8, uint8_t, uint8_t);



// typedef float v256xf32
// __attribute__((vector_size(256 * sizeof(float))));
//
// typedef uint8_t v256xi8
// __attribute__((vector_size(256 * sizeof(uint8_t))));
//

// void lmm_ap1(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_zero(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_inc(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
//
// void lmm_ap2(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_add(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_sub(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_mul(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_div(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_rem(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_eq(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_ne(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_lt(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_gt(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_lte(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_gte(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_min(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_max(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
//
// void lmm_int(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_float(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_bool(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_not(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_and(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_or(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_xor(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
//
// void lmm_fst(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_snd(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);



int8_t my_succ_i8(ptr_t c, int8_t x)
{
  return x + 1;
}
int8_t my_add_i8(ptr_t c, int8_t x, int8_t y)
{
  return x + 1;
}

void test_vm_loop()
{
  lmm_t vm = lmm_create();

  lmm_alloc(vm, 16, 0);
  lmm_alloc(vm, 16, 1);
  lmm_alloc(vm, 16, 2);
  // lmm_alloc(vm, 16, 10);
  // lmm_alloc(vm, 16, 20);

  lmm_set_i8(vm, 1, 0);
  lmm_set_i8(vm, 0, 1);

  doremir_print_ln(lmm_show(vm));

  // lmm_split(vm, 13, 0, 10);
  // lmm_swap(vm, 0, 1);
  // lmm_swap(vm, 3, 0);
  // lmm_swap(vm, 3, 3);

  // lmm_ap1_i8_i8(vm, (unary_t) my_succ_i8, NULL, 0);
  // lmm_ap1_i8_i8(vm, (unary_t) my_succ_i8, NULL, 1);
  // lmm_ap2_i8_i8_i8(vm, (binary_t) my_add_i8, NULL, 0, 1);
  lmm_add_i8_i8(vm, 0, 1);


  doremir_print_ln(lmm_show(vm));
  lmm_destroy(vm);
}

void test_vm()
{
  // while(1)
  test_vm_loop();
}