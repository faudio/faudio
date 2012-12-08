
#ifndef _DOREMIR_THREAD
#define _DOREMIR_THREAD



/** @defgroup Doremir
    @{
    @defgroup Thread
    @{
    */

typedef intptr_t doremir_thread_t;
typedef intptr_t doremir_thread_mutex_t;
typedef intptr_t doremir_thread_condition_t;
doremir_thread_doremir_thread_thread_t doremir_thread_create_thread();
void doremir_thread_join(doremir_thread_doremir_thread_thread_t);
void doremir_thread_detach(doremir_thread_doremir_thread_thread_t);
doremir_thread_lock_t doremir_thread_create_lock();
void doremir_thread_destroy_lock(doremir_thread_lock_t);
bool doremir_thread_lock(doremir_thread_doremir_thread_mutex_t);
bool doremir_thread_try_lock(doremir_thread_doremir_thread_mutex_t);
bool doremir_thread_unlock(doremir_thread_doremir_thread_mutex_t);
doremir_thread_doremir_thread_condition_t doremir_thread_create_condition();
void doremir_thread_wait_for(doremir_thread_doremir_thread_condition_t);
void doremir_thread_notify(doremir_thread_doremir_thread_condition_t);
void doremir_thread_notify_all(doremir_thread_doremir_thread_condition_t);

/** @}
    @}
    */

#endif // _DOREMIR_THREAD

