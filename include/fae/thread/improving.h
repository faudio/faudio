
#ifndef _FAE_THREAD_IMPROVING
#define _FAE_THREAD_IMPROVING

#include <fae/std.h>
#include <fae/thread.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeThread Thread
    @{
    @defgroup FaeThreadImproving Improving
    @{
    */

typedef struct _fae_thread_improving_t * fae_thread_improving_t;
typedef fae_ptr_t fae_thread_improving_value_t;
fae_thread_improving_t fae_thread_improving_create();
void fae_thread_improving_destroy(fae_thread_improving_t);
bool fae_thread_improving_is_done(fae_thread_improving_t);
void fae_thread_improving_wait(fae_thread_improving_t);
fae_thread_improving_value_t fae_thread_improving_get(fae_thread_improving_t);

/** @}
    @}
    @}
    */

#endif // _FAE_THREAD_IMPROVING

