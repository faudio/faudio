
#ifndef _FAE_THREAD_FUTURE
#define _FAE_THREAD_FUTURE

#include <fae/std.h>
#include <fae/thread.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeThread Thread
    @{
    @defgroup FaeThreadFuture Future
    @{
    */

typedef struct _fae_thread_future_t * fae_thread_future_t;
typedef fae_ptr_t fae_thread_future_value_t;
fae_thread_future_t fae_thread_future_create(fae_nullary_t,
                                             fae_ptr_t);
void fae_thread_future_pause(fae_thread_future_t);
void fae_thread_future_resume(fae_thread_future_t);
bool fae_thread_future_is_done(fae_thread_future_t);
fae_thread_future_value_t fae_thread_future_get(fae_thread_future_t);
void fae_thread_future_cancel(fae_thread_future_t);

/** @}
    @}
    @}
    */

#endif // _FAE_THREAD_FUTURE

