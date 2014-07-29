
#ifndef _FA_THREAD
#define _FA_THREAD

#include <fa.h>
#include <fa/std.h>
#include <fa/time.h>

/** @addtogroup FaThread

    Provides threads.

    We provide threads and mutexes. For non-blocking constructs, see the @ref FaAtomic modules.

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

/** The abstract type of threads.
*/
typedef struct _fa_thread_t * fa_thread_t;

/** The abstract type of mutexes.
*/
typedef struct _fa_thread_mutex_t * fa_thread_mutex_t;

/** Create a new thread executing the given function asynhronously.

    Threads have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @param func Function to execute.
    @param data Value to be passed to the function.
    @return     A new thread executing concurrently with the current thread.
*/
fa_thread_t fa_thread_create(fa_nullary_t nullary, fa_ptr_t ptr);

/** Destroy a thread, and return after its associated function has returned.
    @param thread Thread to join (destroyed).
      
*/
void fa_thread_join(fa_thread_t thread);

/** Destroy a thread and return directly. The associated function may continous executing
    in the background.
    @param thread Thread to detach (destroyed).
      
*/
void fa_thread_detach(fa_thread_t thread);

/** Convert a thread to a the underlying thread type used by the platform.

    * On Mac OS X and iOS, `pthread_t` is used.
*/
fa_ptr_t fa_thread_to_native(fa_thread_t thread);

/** Return the main thread.
      
*/
fa_thread_t fa_thread_main();

/** Return the current thread.
      
*/
fa_thread_t fa_thread_current();

/** Sleep the current thread for the given time.
*/
void fa_thread_sleep(fa_time_milliseconds_t milliseconds);

/** Create a mutex.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destructive function.
*/
fa_thread_mutex_t fa_thread_create_mutex();

/** Destroy a mutex.
*/
void fa_thread_destroy_mutex(fa_thread_mutex_t mutex);

/** Acquire the lock of a mutex.
*/
bool fa_thread_lock(fa_thread_mutex_t mutex);

/** Try acquiring the lock of a mutex.
*/
bool fa_thread_try_lock(fa_thread_mutex_t mutex);

/** Release the lock of a mutex.
*/
bool fa_thread_unlock(fa_thread_mutex_t mutex);

/** @}
    @}
    */

#endif // _FA_THREAD

