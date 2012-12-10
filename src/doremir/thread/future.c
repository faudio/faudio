
#ifndef _DOREMIR_THREAD_FUTURE
#define _DOREMIR_THREAD_FUTURE



/** @defgroup Doremir
    @{
    @defgroup Thread
    @{
    @defgroup Future
    @{
    */

typedef struct _doremir_future_t * doremir_future_t;
typedef intptr_t doremir_thread_future_value_t;
doremir_future_t doremir_thread_future_create();
void doremir_thread_future_destroy(doremir_future_t);
bool doremir_thread_future_is_done(doremir_future_t);
void doremir_thread_future_wait(doremir_future_t);
doremir_thread_future_value_t doremir_thread_future_get(doremir_future_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_THREAD_FUTURE

