
#ifndef _FAE_THREAD
#define _FAE_THREAD

#include <fae.h>
#include <fae/std.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeThread Thread
    @{
    */

typedef struct _fae_thread_t * fae_thread_t;
typedef struct _fae_thread_mutex_t * fae_thread_mutex_t;
typedef int fae_thread_milliseconds_t;
fae_thread_t fae_thread_create(fae_nullary_t, fae_ptr_t);
void fae_thread_sleep(fae_thread_milliseconds_t);
void fae_thread_join(fae_thread_t);
void fae_thread_detach(fae_thread_t);
fae_thread_t fae_thread_main();
fae_thread_t fae_thread_current();
fae_thread_mutex_t fae_thread_create_mutex();
void fae_thread_destroy_mutex(fae_thread_mutex_t);
bool fae_thread_lock(fae_thread_mutex_t);
bool fae_thread_try_lock(fae_thread_mutex_t);
bool fae_thread_unlock(fae_thread_mutex_t);

/** @}
    @}
    */

#endif // _FAE_THREAD

