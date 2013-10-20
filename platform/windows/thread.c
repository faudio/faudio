
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/thread.h>
#include <fa/util.h>
#include <Windows.h>

typedef struct {
    DWORD       (*function)(DWORD);
    DWORD       value;
} fa_closure_t;

struct _fa_thread_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
};

struct _fa_thread_mutex_t {
    impl_t          impl;       //  Interface dispatcher
    LPCRITICAL_SECTION native;
};

struct _fa_thread_condition_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
    fa_thread_mutex_t  mutex;
};

static void fa_thread_fatal(char *msg, int error);

static const long join_interval_k = 50;

static void fa_thread_fatal(char *msg, int error);

// --------------------------------------------------------------------------------

void fa_thread_initialize()
{
}

void fa_thread_terminate()
{
}

// --------------------------------------------------------------------------------


static DWORD WINAPI start(LPVOID x)
{
    fa_closure_t *closure = x;  // CASTING FROM fa_nullary_t
    return closure->function(closure->value);
}

fa_thread_t fa_thread_create(fa_nullary_t closure, fa_ptr_t data)
{
    fa_thread_t thread = malloc(sizeof(struct _fa_thread_t));

    HANDLE result = CreateThread(NULL, 0, start, closure, 0, NULL);

    if (!result) {
        fa_thread_fatal("create", GetLastError());
    }

    thread->native = result;
    return thread;
}

void fa_thread_sleep(fa_time_milliseconds_t millis)
{
    Sleep(millis);
}

void fa_thread_join(fa_thread_t thread)
{
    BOOL result;
    DWORD exitCode;

    do {
        Sleep(join_interval_k);
        result = GetExitCodeThread(thread->native, &exitCode);

        if (!result) {
            fa_thread_fatal("join", GetLastError());
        }

    } while (exitCode == STILL_ACTIVE);

    free(thread);
}

void fa_thread_detach(fa_thread_t thread)
{
    BOOL result = CloseHandle(thread->native);
    free(thread);

    if (!result) {
        fa_thread_fatal("detach", GetLastError());
    }
}


// --------------------------------------------------------------------------------

/** Create a mutex.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
fa_thread_mutex_t fa_thread_create_mutex()
{
    fa_thread_mutex_t mutex = malloc(sizeof(struct _fa_thread_mutex_t));

    LPCRITICAL_SECTION crit_sect = malloc(sizeof(CRITICAL_SECTION));
	
	if(!InitializeCriticalSectionAndSpinCount(crit_sect, 0x00000400)) {
		fa_thread_fatal("create_mutex", GetLastError());
	}

    mutex->native = crit_sect;
    return mutex;
}

/** Destroy a mutex.
 */
void fa_thread_destroy_mutex(fa_thread_mutex_t mutex)
{
	DeleteCriticalSection(mutex->native);
	
	if(mutex->native)
		free(mutex->native);
	
	if(mutex)
		free(mutex);
}

/** Acquire the lock of a mutex.
 */
bool fa_thread_lock(fa_thread_mutex_t mutex)
{
    EnterCriticalSection(mutex->native);
    return true;
}

/** Try acquiring the lock of a mutex.
 */
bool fa_thread_try_lock(fa_thread_mutex_t mutex)
{
    return TryEnterCriticalSection(mutex->native);
}

/** Release the lock of a mutex.
 */
bool fa_thread_unlock(fa_thread_mutex_t mutex)
{
    LeaveCriticalSection(mutex->native);
    return true;
}







// --------------------------------------------------------------------------------

void fa_audio_engine_log_error_from(fa_string_t msg, fa_string_t origin) {
    assert(false && "implementation missing");
}

void fa_thread_fatal(char *msg, int error)
{
    fa_audio_engine_log_error_from(string(msg), string("Doremir.Thread"));
    exit(error);
}

