
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
// Utility
// --------------------------------------------------------------------------------

void doremir_thread_fatal(char* msg, int error)
{
    // TODO log
    printf("Fatal error: Doremir: Thread: %s: %d\n", msg, error);
    exit(error);
}
