
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/thread.h>
#include <Windows.h>

struct _doremir_thread_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
};

struct _doremir_thread_mutex_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
};

struct _doremir_thread_condition_t {
    impl_t          impl;       //  Interface dispatcher
    HANDLE native;
    doremir_thread_mutex_t  mutex;
};

static void doremir_thread_fatal(char *msg, int error);

static const long join_interval_k = 50;


static void doremir_thread_fatal(char *msg, int error);

// --------------------------------------------------------------------------------

void doremir_thread_initialize()
{
}

void doremir_thread_terminate()
{
}

// --------------------------------------------------------------------------------


static DWORD WINAPI start(LPVOID x)
{
    doremir_closure_t *closure = x;
    return closure->function(closure->value);
}

doremir_thread_t doremir_thread_create(doremir_closure_t *closure)
{
    doremir_thread_t thread = malloc(sizeof(struct _doremir_thread_t));

    HANDLE result = CreateThread(NULL, 0, start, closure, 0, NULL);

    if (!result) {
        doremir_thread_fatal("create", GetLastError());
    }

    thread->native = result;
    return thread;
}

void doremir_thread_sleep(doremir_thread_milli_seconds_t millis)
{
    Sleep(millis);
}

void doremir_thread_join(doremir_thread_t thread)
{
    BOOL result;
    DWORD exitCode;

    do {
        Sleep(join_interval_k);
        result = GetExitCodeThread(thread->native, &exitCode);

        if (!result) {
            doremir_thread_fatal("join", GetLastError());
        }

    } while (exitCode == STILL_ACTIVE);

    free(thread);
}

void doremir_thread_detach(doremir_thread_t thread)
{
    BOOL result = CloseHandle(thread->native);
    free(thread);

    if (!result) {
        doremir_thread_fatal("detach", GetLastError());
    }
}


// --------------------------------------------------------------------------------

/** Create a mutex object.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
doremir_thread_mutex_t doremir_thread_create_mutex()
{
    doremir_thread_mutex_t mutex = malloc(sizeof(struct _doremir_thread_mutex_t));

    HANDLE result = CreateMutex(NULL, false, NULL);

    if (!result) {
        doremir_thread_fatal("create_mutex", GetLastError());
    }

    mutex->native = result;
    return mutex;
}

/** Destroy a mutex object.
 */
void doremir_thread_destroy_mutex(doremir_thread_mutex_t mutex)
{
    BOOL result = CloseHandle(mutex->native); // FIXME
    free(mutex);

    if (!result) {
        doremir_thread_fatal("destroy_mutex", GetLastError());
    }
}

/** Acquire the lock of a mutex object.
 */
bool doremir_thread_lock(doremir_thread_mutex_t mutex)
{
    DWORD result = WaitForSingleObject(mutex->native, INFINITE);
    assert(result != WAIT_FAILED);
    return result == WAIT_OBJECT_0;
}

/** Try acquiring the lock of a mutex object.
 */
bool doremir_thread_try_lock(doremir_thread_mutex_t mutex)
{
    DWORD result = WaitForSingleObject(mutex->native, 0);
    assert(result != WAIT_FAILED);
    return result == WAIT_OBJECT_0;
}

/** Release the lock of a mutex object.
 */
bool doremir_thread_unlock(doremir_thread_mutex_t mutex)
{
    BOOL result = ReleaseMutex(mutex->native);
    assert(result != 0);
    return true;
}







// --------------------------------------------------------------------------------

void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin);

void doremir_thread_fatal(char *msg, int error)
{
    doremir_audio_engine_log_error_from(string(msg), string("Doremir.Thread"));
    exit(error);
}

