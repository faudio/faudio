
#ifndef _FA_THREAD
#define _FA_THREAD

#include <fa.h>
#include <fa/std.h>

/** @addtogroup FaThread

    @addtogroup FaThread

    Cross-platform threads.

    We provide threads and mutexes. For non-blocking constructs, see the @ref FaAtomic modules.

    @par Literals
    - `thread(function, data)`

    @par Implements 
    - fa_equal_t  (Thread only)
    - fa_order_t  (Thread only)
    - fa_string_show_t (both)
    - fa_destroy_t (Mutex only)
    
    Threads do not implement destroy, use join or detach instead.
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaThread Thread
    @{
    */


typedef struct _fa_thread_t * fa_thread_t;


typedef struct _fa_thread_mutex_t * fa_thread_mutex_t;


typedef int fa_thread_milliseconds_t;


fa_thread_t fa_thread_create(fa_nullary_t, fa_ptr_t);


void fa_thread_sleep(fa_thread_milliseconds_t);


void fa_thread_join(fa_thread_t);


void fa_thread_detach(fa_thread_t);


fa_thread_t fa_thread_main();


fa_thread_t fa_thread_current();


fa_thread_mutex_t fa_thread_create_mutex();


void fa_thread_destroy_mutex(fa_thread_mutex_t);


bool fa_thread_lock(fa_thread_mutex_t);


bool fa_thread_try_lock(fa_thread_mutex_t);


bool fa_thread_unlock(fa_thread_mutex_t);

/** @}
    @}
    */

#endif // _FA_THREAD

