
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

static void doremir_thread_fatal(char* msg, int error);

static const long doremir_thread_join_interval = 50;


// --------------------------------------------------------------------------------
// Threads
// --------------------------------------------------------------------------------

static DWORD WINAPI doremir_thread_start(LPVOID x) 
{                
    doremir_thread_runnable_t *run = x;
    return run->f(run->x);
    return 0;
}

doremir_thread_t doremir_thread_create(doremir_thread_runnable_t* run)
{
    doremir_thread_t t = malloc(sizeof(struct _doremir_thread_t));
    
    HANDLE r = CreateThread(NULL, 0, doremir_thread_start, run, 0, NULL);
    if (r == NULL)
    {        
        doremir_thread_fatal("doremir_thread_create", GetLastError());
    }
    t->native = r;
    return t;
}

void doremir_thread_sleep(doremir_thread_milli_seconds_t s)
{
    Sleep(s);
}

void doremir_thread_join(doremir_thread_t t)
{         
    BOOL r;
    DWORD c;
    do
    {
        Sleep(doremir_thread_join_interval);
        r = GetExitCodeThread(t->native, &c);
        if (!r)
        {
            doremir_thread_fatal("doremir_thread_join", GetLastError());            
        }
    } while (c == STILL_ACTIVE);
    free(t);                
}

void doremir_thread_detach(doremir_thread_t t)
{
    BOOL r = CloseHandle(t->native);
    free(t);
    if (!r)
    {
        doremir_thread_fatal("doremir_thread_detach", GetLastError());
    }
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

void doremir_thread_fatal(char* msg, int error)
{               
    // TODO log
    printf("DoReMIR audio fatal error: %s: %d\n", msg, error);
    exit(error);
}
