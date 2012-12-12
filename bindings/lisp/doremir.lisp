
#ifndef _DOREMIR
#define _DOREMIR

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    */

typedef void * doremir_ptr_t;
typedef doremir_ptr_t (* doremir_nullary_t)();
typedef doremir_ptr_t (* doremir_unary_t)(doremir_ptr_t);
typedef doremir_ptr_t (* doremir_binary_t)(doremir_ptr_t,
                                           doremir_ptr_t);
typedef struct {
            intptr_t (* function)(intptr_t); intptr_t value;
        } doremir_closure_t;

/** @}
    */

#endif // _DOREMIR

