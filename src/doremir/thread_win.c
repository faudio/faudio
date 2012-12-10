
#include <Windows.h>
#include <doremir/thread.h>

struct _doremir_thread_t
{
    HANDLE native;
};

struct _doremir_thread_mutex_t
{
    HANDLE native;
};

struct _doremir_thread_condition_t
{
    HANDLE native;
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
doremir_thread_t doremir_thread_create(doremir_thread_runnable_t run)
{
    doremir_thread_t t = malloc(sizeof(struct _doremir_thread_t));
    int r = CreateThread(
        NULL,
        0,
        run.f, // TODO type?
        run.x,
        0
        );
    if (r == NULL)
        doremir_thread_fatal(GetLastError());
    t->native = r;
    return t;
}

void doremir_thread_sleep(doremir_thread_milli_seconds_t s)
{
    Sleep(s);
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








doremir_thread_mutex_t doremir_thread_create_mutex()
{
    doremir_thread_mutex_t m = malloc(sizeof(struct _doremir_thread_mutex_t));
    int r = 0;
    if (r != 0)
        ; // TODO fatal
    return m;
}

void doremir_thread_destroy_mutex(doremir_thread_mutex_t m)
{                     
    int r = 0;
    free(m);
    if (r != 0)
        ; // TODO fatal
}

bool doremir_thread_lock(doremir_thread_mutex_t t)
{ 
    int r = 0;
    if (r != 0)
        ; // TODO fatal
}

bool doremir_thread_try_lock(doremir_thread_mutex_t t)
{
    int r = 0;
    if (r != 0)
        ; // TODO fatal
}

bool doremir_thread_unlock(doremir_thread_mutex_t t)
{
    int r = 0;
    if (r != 0)
        ; // TODO fatal
}

doremir_thread_condition_t doremir_thread_create_condition(doremir_thread_mutex_t m)
{
    int r = 0;
    if (r != 0)
        ; // TODO fatal
    // TODO return c
}

void doremir_thread_destroy_condition(doremir_thread_condition_t c)
{
    int r = 0;
    if (r != 0)
        ; // TODO fatal
    // TODO return c    
}

void doremir_thread_wait_for(doremir_thread_condition_t c)
{
    int r = 0;
    // TODO free
    if (r != 0)
        ; // TODO fatal
}

void doremir_thread_notify(doremir_thread_condition_t c)
{
    int r = 0;
    if (r != 0)
        ; // TODO fatal
}

void doremir_thread_notify_all(doremir_thread_condition_t c)
{
    int r = 0;
    if (r != 0)
        ; // TODO fatal
}


// --------------------------------------------------------------------------------
// Utility
// --------------------------------------------------------------------------------

void doremir_thread_fatal(int error)
{               
    // TODO log
    exit(error);
}
