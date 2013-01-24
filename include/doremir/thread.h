
#ifndef _DOREMIR_THREAD
#define _DOREMIR_THREAD

#include <doremir.h>
#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirThread Thread
    @{
    */

typedef struct _doremir_thread_t * doremir_thread_t;
typedef struct _doremir_thread_mutex_t * doremir_thread_mutex_t;
typedef struct _doremir_thread_condition_t * doremir_thread_condition_t;
typedef int doremir_thread_millis_t;
doremir_thread_t doremir_thread_create(doremir_nullary_t,
                                       doremir_ptr_t);
void doremir_thread_sleep(doremir_thread_millis_t);
void doremir_thread_join(doremir_thread_t);
void doremir_thread_detach(doremir_thread_t);
doremir_thread_mutex_t doremir_thread_create_mutex();
void doremir_thread_destroy_mutex(doremir_thread_mutex_t);
bool doremir_thread_lock(doremir_thread_mutex_t);
bool doremir_thread_try_lock(doremir_thread_mutex_t);
bool doremir_thread_unlock(doremir_thread_mutex_t);
doremir_thread_condition_t doremir_thread_create_condition(doremir_thread_mutex_t);
void doremir_thread_destroy_condition(doremir_thread_condition_t);
void doremir_thread_wait_for(doremir_thread_condition_t);
void doremir_thread_notify(doremir_thread_condition_t);
void doremir_thread_notify_all(doremir_thread_condition_t);

/** @}
    @}
    */

#endif // _DOREMIR_THREAD

