
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/string.h>
#import <doremir/util.h>

/*
    Little Matrix Machine (LMM)

    - Matrix registers: [0..2^10-1]

    - Each matrix register contains a raw buffer of known size.

        - Buffers are allocated using the alloc opcode.

            - Initially, all buffers have *max size* and *size* 0.
            - Buffer *max size* is constant until swap, or alloc is called.
            - Buffer *size* is affected by data ops as follows:
                - Using copy, split, ap1 or ap2 with a too small buffer will
                  fail.
                - All other opcodes use vector arithmetic, truncating to the
                  shortest argument size.

    - Convenience registers:
        off, step, loop

    - Instructions
        - Unfolding operations stores result in second operand while folding
          operations (most) store in the first.

        - Size arguments are in bytes.


        - Full table
            alloc         s r
            swap          r1 r2

            copy          r1 r2              r1                      ==> r2
            split         s r1 r2            (r1[0..s-1],r1[s..n-1]) ==> (r1,r2)

            set   T1      x r1               r1[i] <== x                    where i <- [0..n]
            ap1   T1      f ct r1            r1[i] <== f(ct, r1[i])         where i <- [0..n]
            ap2   T1 T2   f ct r1 r2         r1[i] <== f(ct, r1[i], r2[i])  where i <- [0..n]

            inc   T1      r1                 r1    <== r1 + 1
            dec   T1      r1                 r1    <== r1 - 1
            neg   T1      r1                 r1    <== r1 * (-1)
            int   T1      r1                 r1    <== (T) (long int) r1
            float T1      r12                r1    <== (T) (double)   r1
            bool  T1      r1                 r1    <== case r1 of 0 -> 0, _ -> 1
            not   T1      r1                 r1    <== case r1 of 0 -> 1, _ -> 0

            add   T1      r1 r2              r1    <== r1 + r2
            sub   T1      r1 r2              r1    <== r1 - r2
            mul   T1      r1 r2              r1    <== r1 * r2
            div   T1      r1 r2              r1    <== r1 / r2
            rem   T1      r1 r2              r1    <== r1 % r2
            and   T1      r1 r2              r1    <== r1 & r2
            or    T1      r1 r2              r1    <== r1 | r2
            xor   T1      r1 r2              r1    <== r1 ^ r2

            eq    T1      r1 r2              r1    <== r1 == r2
            ne    T1      r1 r2              r1    <== r1 != r2
            lt    T1      r1 r2              r1    <== r1 < r2
            gt    T1      r1 r2              r1    <== r1 > r2
            lte   T1      r1 r2              r1    <== r1 <= r2
            gte   T1      r1 r2              r1    <== r1 >= r2

 */

enum { kLmmRegs = 1024 };

struct lmm {
    struct {
        size_t size;
        size_t maxSize;
        void * data;
    } regs[kLmmRegs];

    size_t off;
    size_t step;
    size_t loops;

    char * error;
};

typedef struct lmm * lmm_t;
typedef uint16_t lmm_reg_t;

lmm_t   lmm_create();
void    lmm_destroy(lmm_t lmm);
char  * lmm_get_error(lmm_t lmm);
size_t  lmm_get_reg_size(lmm_t lmm, lmm_reg_t r);
size_t  lmm_get_reg_max_size(lmm_t lmm, lmm_reg_t r);
void  * lmm_get_reg_data(lmm_t lmm, lmm_reg_t r);


void lmm_alloc(lmm_t lmm, size_t size, lmm_reg_t r);
void lmm_copy(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
void lmm_swap(lmm_t lmm, lmm_reg_t r1, lmm_reg_t r2);
void lmm_split(lmm_t lmm, size_t size, lmm_reg_t r1, lmm_reg_t r2);

void lmm_set_i8(lmm_t lmm, uint8_t x, lmm_reg_t r1);
void lmm_set_i16(lmm_t lmm, uint16_t x, lmm_reg_t r1);
void lmm_set_i32(lmm_t lmm, uint32_t x, lmm_reg_t r1);
void lmm_set_i64(lmm_t lmm, uint64_t x, lmm_reg_t r1);
void lmm_set_f32(lmm_t lmm, float x, lmm_reg_t r1);
void lmm_set_f64(lmm_t lmm, double x, lmm_reg_t r1);

// void lmm_ap1_i8_i8(lmm_t lmm, unary_t f, ptr_t ct, lmm_reg_t r1, lmm_reg_t r2);
// void lmm_ap1_f32_f32(lmm_t lmm, unary_t f, ptr_t ct, lmm_reg_t r1, lmm_reg_t r2);





