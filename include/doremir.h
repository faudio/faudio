
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
            doremir_ptr_t (* function)(doremir_ptr_t); doremir_ptr_t value;
        } doremir_closure_t;

/** @}
    */

#endif // _DOREMIR

