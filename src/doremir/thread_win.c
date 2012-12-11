
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

static 
DWORD WINAPI doremir_thread_start(LPVOID x) 
{                
    doremir_thread_runnable_t *run = x;
    return run->f(run->x);
    return 0;
}

doremir_thread_t doremir_thread_create(doremir_thread_runnable_t* run)
{
    doremir_thread_t t = malloc(sizeof(struct _doremir_thread_t));
    
    HANDLE r = CreateThread(
        NULL,
        0,
        doremir_thread_start,
        run,
        0,
        NULL
        );
    if (r == NULL)
        doremir_thread_fatal("doremir_thread_create", GetLastError());
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
            doremir_thread_fatal("doremir_thread_join", GetLastError());            
    } while (c == STILL_ACTIVE);
    free(t);                
}

void doremir_thread_detach(doremir_thread_t t)
{
    BOOL r = CloseHandle(t->native);
    free(t);
    if (!r)
        doremir_thread_fatal("doremir_thread_detach", GetLastError());
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
