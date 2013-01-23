
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/string.h>
#import <doremir/util.h>

/*
    Little Matrix Machine

    - Matrix registers: [0..2^16]
        - By convention, we use the upper half (>= 32768) for loops

    - Each matrix register contains a raw buffer
        - Buffers are allocated using the alloc opcode
        - Using dup and split on a too small buffer will fail
        - Using swap flips both max size and actual
        - Operators use matrix arithmetic, truncating to shortest argument

    - Convenience registers:
        off step loops

    - Instructions

            alloc         s r               r1[i]    := 0                    for each i in [0..s]

            dup           r1 r2             (r1,r2)  := r1
            swap          r1 r2             (r1,r2)  := (r2,r1)
            split         s r1 r2           (r1,r2)  := (r1[0..s-1], r1[s..n])

            ap1           f ct r1           r1[i]    := f(ct, r1[i])         for each i in [0..n]
            ap1 i8  i8
            ap1 i8  f32
            ap1 f32 i8
            ap1 f32 f32

            ap2           f ct r1 r2        r1[i]    := f(ct, r1[i], r2[i])  for each i in [0..n]
            ap2 i8  i8   i8
            ap2 i8  f32  i8
            ap2 f32 i8   i8
            ap2 f32 f32  i8
            ap2 i8  i8   f32
            ap2 i8  f32  f32
            ap2 f32 i8   f32
            ap2 f32 f32  f32

            add           r1 r2              r1 := r1 + r2
            sub           r1 r2              r1 := r1 - r2
            mul           r1 r2              r1 := r1 * r2
            div           r1 r2              r1 := r1 / r2
            rem           r1 r2              r1 := r1 % r2
            eq            r1 r2
            ne            r1 r2
            gt            r1 r2
            lt            r1 r2
            gte           r1 r2
            lte           r1 r2
            min           r1 r2
            max           r1 r2

            int           r1 r2              cast to long int, then narrow
            float         r1 r2              cast to double, then narrow
            bool          r1 r2              0 -> 0, _ -> 1
            not           r1 r2              0 -> 1, _ -> 0

            and           r1 r2              bitwise
            or            r1 r2              bitwise
            xor           r1 r2              bitwise

            fst           r1 r2              r1 := r1
            snd           r1 r2              r1 := r2

 */

enum { kLmmRegs = 65536 };

struct lmm
{
    struct
    {
        size_t size;
        size_t maxSize;
        void  *data;
    } regs[kLmmRegs];

    size_t off;
    size_t step;
    size_t loops;

    char *error;
};
typedef struct lmm *lmm_t;
typedef uint16_t reg_t;

lmm_t lmm_create();
void lmm_destroy(lmm_t lmm);
char *lmm_get_error(lmm_t lmm);
size_t lmm_get_reg_size(lmm_t lmm, reg_t r);
void *lmm_get_reg_data(lmm_t lmm, reg_t r);

void lmm_alloc(lmm_t lmm, size_t size, reg_t r);


void lmm_set_i8(lmm_t lmm, uint8_t x, reg_t r1);

void lmm_dup(lmm_t lmm, reg_t r1, reg_t r2);
void lmm_swap(lmm_t lmm, reg_t r1, reg_t r2);
void lmm_split(lmm_t lmm, size_t size, reg_t r1, reg_t r2);

void lmm_ap1_i8_i8(lmm_t lmm, unary_t f, ptr_t ct, reg_t r1, reg_t r2);
void lmm_ap1_f32_f32(lmm_t lmm, unary_t f, ptr_t ct, reg_t r1, reg_t r2);





