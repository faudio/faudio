
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/thread.h>
#include <Windows.h>

struct _fae_thread_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
};

struct _fae_thread_mutex_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
};

struct _fae_thread_condition_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
    fae_thread_mutex_t  mutex;
};

static void fae_thread_fatal(char *msg, int error);

static const long join_interval_k = 50;


static void fae_thread_fatal(char *msg, int error);

// --------------------------------------------------------------------------------

void fae_thread_initialize()
{
}

void fae_thread_terminate()
{
}

// --------------------------------------------------------------------------------


static DWORD WINAPI start(LPVOID x)
{
    fae_closure_t *closure = x;
    return closure->function(closure->value);
}

fae_thread_t fae_thread_create(fae_closure_t *closure)
{
    fae_thread_t thread = malloc(sizeof(struct _fae_thread_t));

    HANDLE result = CreateThread(NULL, 0, start, closure, 0, NULL);

    if (!result) {
        fae_thread_fatal("create", GetLastError());
    }

    thread->native = result;
    return thread;
}

void fae_thread_sleep(fae_thread_milli_seconds_t millis)
{
    Sleep(millis);
}

void fae_thread_join(fae_thread_t thread)
{
    BOOL result;
    DWORD exitCode;

    do {
        Sleep(join_interval_k);
        result = GetExitCodeThread(thread->native, &exitCode);

        if (!result) {
            fae_thread_fatal("join", GetLastError());
        }

    } while (exitCode == STILL_ACTIVE);

    free(thread);
}

void fae_thread_detach(fae_thread_t thread)
{
    BOOL result = CloseHandle(thread->native);
    free(thread);

    if (!result) {
        fae_thread_fatal("detach", GetLastError());
    }
}


// --------------------------------------------------------------------------------

/** Create a mutex.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
fae_thread_mutex_t fae_thread_create_mutex()
{
    fae_thread_mutex_t mutex = malloc(sizeof(struct _fae_thread_mutex_t));

    HANDLE result = CreateMutex(NULL, false, NULL);

    if (!result) {
        fae_thread_fatal("create_mutex", GetLastError());
    }

    mutex->native = result;
    return mutex;
}

/** Destroy a mutex.
 */
void fae_thread_destroy_mutex(fae_thread_mutex_t mutex)
{
    BOOL result = CloseHandle(mutex->native); // FIXME
    free(mutex);

    if (!result) {
        fae_thread_fatal("destroy_mutex", GetLastError());
    }
}

/** Acquire the lock of a mutex.
 */
bool fae_thread_lock(fae_thread_mutex_t mutex)
{
    DWORD result = WaitForSingleObject(mutex->native, INFINITE);
    assert(result != WAIT_FAILED);
    return result == WAIT_OBJECT_0;
}

/** Try acquiring the lock of a mutex.
 */
bool fae_thread_try_lock(fae_thread_mutex_t mutex)
{
    DWORD result = WaitForSingleObject(mutex->native, 0);
    assert(result != WAIT_FAILED);
    return result == WAIT_OBJECT_0;
}

/** Release the lock of a mutex.
 */
bool fae_thread_unlock(fae_thread_mutex_t mutex)
{
    BOOL result = ReleaseMutex(mutex->native);
    assert(result != 0);
    return true;
}







// --------------------------------------------------------------------------------

void fae_audio_engine_log_error_from(fae_string_t msg, fae_string_t origin);

void fae_thread_fatal(char *msg, int error)
{
    fae_audio_engine_log_error_from(string(msg), string("Doremir.Thread"));
    exit(error);
}

