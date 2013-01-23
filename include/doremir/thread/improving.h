
#ifndef _DOREMIR_THREAD_IMPROVING
#define _DOREMIR_THREAD_IMPROVING

#include <doremir/std.h>
#include <doremir/thread.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirThread Thread
    @{
    @defgroup DoremirThreadImproving Improving
    @{
    */

typedef struct _doremir_thread_improving_t *doremir_thread_improving_t;
typedef doremir_ptr_t doremir_thread_improving_value_t;
doremir_thread_improving_t doremir_thread_improving_create();
void doremir_thread_improving_destroy(doremir_thread_improving_t);
bool doremir_thread_improving_is_done(doremir_thread_improving_t);
void doremir_thread_improving_wait(doremir_thread_improving_t);
doremir_thread_improving_value_t doremir_thread_improving_get(doremir_thread_improving_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_THREAD_IMPROVING

