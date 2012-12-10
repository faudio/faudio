
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

static void doremir_thread_fatal(int error);


// --------------------------------------------------------------------------------
// Threads
// --------------------------------------------------------------------------------

/** Create a new thread executing the given function asynhronously.

    Threads have single-ownership semantics and must be finalized by passing it
    to a destroy function.    
 */
doremir_thread_t doremir_thread_create(doremir_thread_runnable_t* run)
{
    doremir_thread_t t = malloc(sizeof(struct _doremir_thread_t));
    int r = pthread_create(&t->native, NULL, (void*(*)(void*)) run->f, (void*)run->x);
    if (r != 0)
        doremir_thread_fatal(r);
    return t;
}

void doremir_thread_sleep(doremir_thread_milli_seconds_t s)
{
    usleep(s * 1000);
}

/** Destroy a thread, and return after its associated function has returned.
  */
void doremir_thread_join(doremir_thread_t t)
{
    int r = pthread_join(t->native, NULL);
    free(t);
    if (r != 0)
        doremir_thread_fatal(r);
}

/** Destroy a thread and return directly. The associated function may continous executing
    in the background.
  */
void doremir_thread_detach(doremir_thread_t t)
{
    int r = pthread_detach(t->native);
    free(t);
    if (r != 0)
        doremir_thread_fatal(r);
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
    doremir_thread_mutex_t m = malloc(sizeof(struct _doremir_thread_mutex_t));
    int r = pthread_mutex_init(&m->native, NULL);
    if (r != 0)
        doremir_thread_fatal(r);
    return m;
}

/** Destroy a mutex object.
 */
void doremir_thread_destroy_mutex(doremir_thread_mutex_t m)
{                     
    int r = pthread_mutex_destroy(&m->native);
    free(m);
    if (r != 0)
        doremir_thread_fatal(r);
}

/** Acquire the lock of a mutex object.
 */
bool doremir_thread_lock(doremir_thread_mutex_t t)
{ 
    int r = 0;
    if (r != 0)
        doremir_thread_fatal(r);
    return true;
}

/** Try acquiring the lock of a mutex object.
 */
bool doremir_thread_try_lock(doremir_thread_mutex_t t)
{
    int r = 0;
    if (r != 0)
        doremir_thread_fatal(r);
    return true; // TODO
}

/** Release the lock of a mutex object.
 */
bool doremir_thread_unlock(doremir_thread_mutex_t t)
{
    int r = 0;
    if (r != 0)
        doremir_thread_fatal(r);
    return true; // TODO
}


// --------------------------------------------------------------------------------
// Conditions
// --------------------------------------------------------------------------------

/** Create a condition object.   

    Conditions have single-ownership semantics and must be finalized by passing it
    to a destroy function.        
 */
doremir_thread_condition_t doremir_thread_create_condition(doremir_thread_mutex_t m)
{
    doremir_thread_condition_t c = malloc(sizeof(struct _doremir_thread_condition_t));
    c->mutex = m;
    int r = pthread_cond_init(&c->native, NULL);
    if (r != 0)
        doremir_thread_fatal(r);
    return c;
}

void doremir_thread_destroy_condition(doremir_thread_condition_t c)
{           
    int r = pthread_cond_destroy(&c->native);
    free(c);
    if (r != 0)
        doremir_thread_fatal(r);
}

/** Wait for a condition to be signaled.        
 */
void doremir_thread_wait_for(doremir_thread_condition_t c)
{
    int r = pthread_cond_wait(&c->native, &c->mutex->native);
    if (r != 0)
        doremir_thread_fatal(r);
}

/** Signal a condition to one listener.        
 */
void doremir_thread_notify(doremir_thread_condition_t c)
{
    int r = pthread_cond_signal(&c->native);
    if (r != 0)
        doremir_thread_fatal(r);
}

/** Signal a condition to all listeners.
 */
void doremir_thread_notify_all(doremir_thread_condition_t c)
{
    int r = pthread_cond_broadcast(&c->native);
    if (r != 0)
        doremir_thread_fatal(r);
}


// --------------------------------------------------------------------------------
// Utility
// --------------------------------------------------------------------------------

void doremir_thread_fatal(int error)
{               
    // TODO log
    exit(error);
}


