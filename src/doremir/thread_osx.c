
#include <pthread.h>
#include <unistd.h>
#include <doremir/thread.h>

struct _doremir_thread_t
{
    pthread_t native;
};

struct _doremir_thread_mutex_t
{
    pthread_mutex_t native;
};

struct _doremir_thread_condition_t
{
    pthread_cond_t          native;
    doremir_thread_mutex_t  mutex;
};

static void doremir_thread_fatal(char* msg, int error);


// --------------------------------------------------------------------------------
// Threads
// --------------------------------------------------------------------------------

/** Create a new thread executing the given function asynhronously.

    Threads have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
doremir_thread_t doremir_thread_create(doremir_thread_runnable_t* run)
{
    doremir_thread_t thread = malloc(sizeof(struct _doremir_thread_t));

    int result = pthread_create(&thread->native, NULL,
        (void*(*)(void*)) run->f, (void*) run->x);

    if (result != 0)
        doremir_thread_fatal("create", result);

    return thread;
}

/** Sleep the current thread for the given time.
 */
void doremir_thread_sleep(doremir_thread_milli_seconds_t s)
{
    usleep(s * 1000);
}

/** Destroy a thread, and return after its associated function has returned.
  */
void doremir_thread_join(doremir_thread_t thread)
{
    int result = pthread_join(thread->native, NULL);
    free(thread);

    if (result != 0)
        doremir_thread_fatal("join", result);
}

/** Destroy a thread and return directly. The associated function may continous executing
    in the background.
  */
void doremir_thread_detach(doremir_thread_t thread)
{
    int result = pthread_detach(thread->native);
    free(thread);

    if (result != 0)
        doremir_thread_fatal("detach", result);
}


// --------------------------------------------------------------------------------
// Mutexes
// --------------------------------------------------------------------------------

/** Create a mutex object.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
doremir_thread_mutex_t doremir_thread_create_mutex()
{
    doremir_thread_mutex_t mutex = malloc(sizeof(struct _doremir_thread_mutex_t));

    int result = pthread_mutex_init(&mutex->native, NULL);

    if (result != 0)
        doremir_thread_fatal("create_mutex", result);
    return mutex;
}

/** Destroy a mutex object.
 */
void doremir_thread_destroy_mutex(doremir_thread_mutex_t mutex)
{
    int result = pthread_mutex_destroy(&mutex->native);
    free(mutex);

    if (result != 0)
        doremir_thread_fatal("destroy_mutex", result);
}

/** Acquire the lock of a mutex object.
 */
bool doremir_thread_lock(doremir_thread_mutex_t mutex)
{
    int result = pthread_mutex_lock(&mutex->native);

    if (result == 0)
        return true;
    else
    {
        doremir_thread_fatal("unlock", result);
        assert(false);
    }
}

/** Try acquiring the lock of a mutex object.
 */
bool doremir_thread_try_lock(doremir_thread_mutex_t mutex)
{
    int result = pthread_mutex_trylock(&mutex->native);

    switch (result)
    {
    case 0:
        return true;
    case EBUSY:
        return false;
    default:
        doremir_thread_fatal("try_lock", result);
    }
}

/** Release the lock of a mutex object.
 */
bool doremir_thread_unlock(doremir_thread_mutex_t mutex)
{
    int result = pthread_mutex_unlock(&mutex->native);

    if (result == 0)
        return true;
    else
    {
        doremir_thread_fatal("unlock", result);
        assert(false);
    }
}


// --------------------------------------------------------------------------------
// Conditions
// --------------------------------------------------------------------------------

/** Create a condition object.

    Conditions have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
doremir_thread_condition_t doremir_thread_create_condition(doremir_thread_mutex_t mutex)
{
    doremir_thread_condition_t cond = malloc(sizeof(struct _doremir_thread_condition_t));
    cond->mutex = mutex;

    int result = pthread_cond_init(&cond->native, NULL);

    if (result != 0)
        doremir_thread_fatal("create_condition", result);
    return cond;
}

/** Destroy a condition object.
 */
void doremir_thread_destroy_condition(doremir_thread_condition_t cond)
{
    int result = pthread_cond_destroy(&cond->native);
    free(cond);

    if (result != 0)
        doremir_thread_fatal("destroy_condition", result);
}

/** Wait for a condition to be signaled.
 */
void doremir_thread_wait_for(doremir_thread_condition_t cond)
{
    int result = pthread_cond_wait(&cond->native, &cond->mutex->native);

    if (result != 0)
        doremir_thread_fatal("wait_for", result);
}

/** Signal a condition to one listener.
 */
void doremir_thread_notify(doremir_thread_condition_t cond)
{
    int result = pthread_cond_signal(&cond->native);

    if (result != 0)
        doremir_thread_fatal("notify", result);
}

/** Signal a condition to all listeners.
 */
void doremir_thread_notify_all(doremir_thread_condition_t cond)
{
    int result = pthread_cond_broadcast(&cond->native);

    if (result != 0)
        doremir_thread_fatal("notify_all", result);
}


// --------------------------------------------------------------------------------
// Utility
// --------------------------------------------------------------------------------

void doremir_thread_fatal(char* msg, int error)
{
    // TODO log
    printf("Fatal error: Doremir: Thread: %s: %d\n", msg, error);
    exit(error);
}


