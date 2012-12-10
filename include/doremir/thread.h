
#ifndef _DOREMIR_THREAD
#define _DOREMIR_THREAD

#include <doremir/std.h>

/** @defgroup Doremir
    @{
    @defgroup Thread
    @{
    */

typedef struct {
            intptr_t (* f)(intptr_t); intptr_t x;
        } doremir_thread_runnable_t;
typedef struct _doremir_thread_t * doremir_thread_t;
typedef struct _doremir_thread_mutex_t * doremir_thread_mutex_t;
typedef struct _doremir_thread_condition_t * doremir_thread_condition_t;
doremir_thread_t doremir_thread_create(doremir_thread_runnable_t *);
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

