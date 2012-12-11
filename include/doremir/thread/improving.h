
#ifndef _DOREMIR_THREAD_IMPROVING
#define _DOREMIR_THREAD_IMPROVING

#include <doremir/std.h>

/** @defgroup Doremir
    @{
    @defgroup Thread
    @{
    @defgroup Improving
    @{
    */

typedef struct _doremir_improving_t * doremir_improving_t;
typedef intptr_t doremir_thread_improving_value_t;
doremir_improving_t doremir_thread_improving_create();
void doremir_thread_improving_destroy(doremir_improving_t);
bool doremir_thread_improving_is_done(doremir_improving_t);
void doremir_thread_improving_wait(doremir_improving_t);
doremir_thread_improving_value_t doremir_thread_improving_get(doremir_improving_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_THREAD_IMPROVING

