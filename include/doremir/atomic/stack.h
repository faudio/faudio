
#ifndef _DOREMIR_ATOMIC_STACK
#define _DOREMIR_ATOMIC_STACK



/** @defgroup Doremir
    @{
    @defgroup Atomic
    @{
    @defgroup Stack
    @{
    */

typedef struct _doremir_atomic_stack_t * doremir_atomic_stack_t;
doremir_atomic_stack_t doremir_atomic_stack_create();
void doremir_atomic_stack_destroy(doremir_atomic_stack_t);
intptr_t doremir_atomic_stack_read(doremir_atomic_stack_t);
intptr_t doremir_atomic_stack_write(doremir_atomic_stack_t,
                                    intptr_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_ATOMIC_STACK

