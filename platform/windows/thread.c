
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/thread.h>
#include <fa/util.h>
#include <Windows.h>

struct _fa_closure_t {
    DWORD (*function)(DWORD);
    DWORD       value;
};

typedef struct _fa_closure_t *fa_closure_t;

struct _fa_thread_t {
    fa_impl_t impl;               //  Interface dispatcher
    HANDLE    native;             //  Handle
    DWORD     tId;                //  Id
    fa_nullary_t function;
    fa_ptr_t     value;
    fa_string_t  name;
    bool         started;
};

struct _fa_thread_mutex_t {
    fa_impl_t          impl;       //  Interface dispatcher
    LPCRITICAL_SECTION native;
};

static HANDLE main_thread_g = INVALID_HANDLE_VALUE;

static void fa_thread_fatal(char *msg, int error);
static const long join_interval_k = 50;
fa_ptr_t thread_impl(fa_id_t iface);
fa_ptr_t mutex_impl(fa_id_t iface);

// --------------------------------------------------------------------------------

void fa_thread_initialize()
{
    /*
    http://weseetips.com/2008/03/26/getcurrentthread-returns-pseudo-handle-not-the-real-handle/
    */
    if (!DuplicateHandle(
                GetCurrentProcess(),
                GetCurrentThread(),
                GetCurrentProcess(),
                &main_thread_g,
                0,
                true,
                DUPLICATE_SAME_ACCESS)) {
        fa_thread_fatal("duplicate_main", GetLastError());
    }
}

void fa_thread_terminate()
{
    main_thread_g = INVALID_HANDLE_VALUE;
}

inline static fa_thread_t new_thread()
{
    fa_thread_t thread = fa_new(thread);
    thread->impl = &thread_impl;
    return thread;
}
inline static void delete_thread(fa_thread_t thread)
{
    fa_delete(thread);
}

// --------------------------------------------------------------------------------


static DWORD WINAPI thread_proc(LPVOID x)
{
#ifdef __MINGW32__
    fa_thread_t thread = x;
    fa_string_t name = thread->name;
    DWORD tId = GetCurrentThreadId();
    thread->tId = tId;
    if (name) {
        fa_inform(fa_dappend(fa_format_integral("New thread 0x%x ", tId), fa_copy(name)));
    }
    assert(thread->tId);
    fa_nullary_t function = thread->function;
    fa_ptr_t value = thread->value;
    
    // Mark thread as started.
    // From now on, we can no longer assume that the thread structure is valid
    thread->started = true;
    
    // Run the actual thread function
    fa_ptr_t result = function(value);

    // Log that we've stopped
    if (name) {
        fa_inform(fa_dappend(fa_format_integral("Finished thread 0x%x ", tId), name)); // destroys name
    }
    return (DWORD)result;
#else
#error "only 32 bit"
#endif
}

fa_thread_t fa_thread_create(fa_nullary_t function, fa_ptr_t value, fa_string_t name)
{
    fa_thread_t thread  = new_thread();

    thread->impl        = &thread_impl;
    thread->native      = INVALID_HANDLE_VALUE;
    thread->tId         = 0;
    thread->function    = function;
    thread->value       = value;
    //thread->name        = name; // for now, only print the name
    thread->name        = name;
    thread->started     = false;

    HANDLE result = CreateThread(NULL, 0, thread_proc, thread, 0, NULL);

    if (!result) {
        fa_thread_fatal("create", GetLastError());
    }

    thread->native = result;

    // Give thread time to start, otherwise there is a race condition if
    // the calling code detaches the thread immediately after creating it
    while (!thread->started) {
        Sleep(join_interval_k);
    }
 
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

    // TODO waitForSingleObject

    // TODO prevent double free (or tolerate somehow?)
    CloseHandle(thread->native);
    fa_free(thread);
}

void fa_thread_detach(fa_thread_t thread)
{
    BOOL result = CloseHandle(thread->native);
    fa_free(thread);

    if (!result) {
        fa_thread_fatal("detach", GetLastError());
    }
}

fa_thread_t fa_thread_main()
{
    assert((main_thread_g != INVALID_HANDLE_VALUE)
           && "Module not initialized");

    fa_thread_t thread = new_thread();
    thread->impl    = &thread_impl;
    thread->native  = main_thread_g;
    thread->tId     = (DWORD)0;
    return thread;
}

fa_thread_t fa_thread_current()
{
    /*
    http://weseetips.com/2008/03/26/getcurrentthread-returns-pseudo-handle-not-the-real-handle/
    */
    fa_thread_t thread = new_thread();
    thread->impl = &thread_impl;
    thread->tId = (DWORD)0;

    if (!DuplicateHandle(
                GetCurrentProcess(),
                GetCurrentThread(),
                GetCurrentProcess(),
                &thread->native,
                0,
                true,
                DUPLICATE_SAME_ACCESS)) {
        fa_thread_fatal("duplicate_handle", GetLastError());
    }

    return thread;
}




// --------------------------------------------------------------------------------

/** Create a mutex.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destroy function.
 */
fa_thread_mutex_t fa_thread_create_mutex()
{
    fa_thread_mutex_t mutex = fa_malloc(sizeof(struct _fa_thread_mutex_t));

    LPCRITICAL_SECTION crit_sect = fa_malloc(sizeof(CRITICAL_SECTION));

    if (!InitializeCriticalSectionAndSpinCount(crit_sect, 0x00000400)) {
        fa_thread_fatal("create_mutex", GetLastError());
    }

    mutex->impl     = &mutex_impl;
    mutex->native   = crit_sect;
    return mutex;
}

/** Destroy a mutex.
 */
void fa_thread_destroy_mutex(fa_thread_mutex_t mutex)
{
    DeleteCriticalSection(mutex->native);

    if (mutex->native) {
        fa_free(mutex->native);
    }

    if (mutex) {
        fa_free(mutex);
    }
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

bool thread_equal(fa_ptr_t m, fa_ptr_t n)
{
    fa_thread_t x = (fa_thread_t) m;
    fa_thread_t y = (fa_thread_t) n;

    return x->tId == y->tId;
}

bool thread_less_than(fa_ptr_t m, fa_ptr_t n)
{
    fa_thread_t x = (fa_thread_t) m;
    fa_thread_t y = (fa_thread_t) n;

    return x->tId < y->tId;
}

bool thread_greater_than(fa_ptr_t m, fa_ptr_t n)
{
    fa_thread_t x = (fa_thread_t) m;
    fa_thread_t y = (fa_thread_t) n;

    return x->tId > y->tId;
}

fa_string_t thread_show(fa_ptr_t a)
{
    fa_thread_t x = (fa_thread_t) a;

    fa_string_t str = fa_string("<Thread ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) x->tId));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

fa_ptr_t thread_impl(fa_id_t iface)
{
    static fa_equal_t thread_equal_impl
        = { thread_equal };
    static fa_order_t thread_order_impl
        = { thread_less_than, thread_greater_than };
    static fa_string_show_t thread_show_impl
        = { thread_show };

    switch (iface) {
    case fa_equal_i:
        return &thread_equal_impl;

    case fa_order_i:
        return &thread_order_impl;

    case fa_string_show_i:
        return &thread_show_impl;

    default:
        return NULL;
    }
}

fa_string_t mutex_show(fa_ptr_t a)
{
    fa_string_t str = fa_string("<Mutex ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

void mutex_destroy(fa_ptr_t a)
{
    fa_thread_destroy_mutex(a);
}

fa_ptr_t mutex_impl(fa_id_t iface)
{
    static fa_string_show_t mutex_show_impl
        = { mutex_show };
    static fa_destroy_t mutex_destroy_impl
        = { mutex_destroy };

    switch (iface) {
    case fa_string_show_i:
        return &mutex_show_impl;

    case fa_destroy_i:
        return &mutex_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------



void fa_log_error_from(fa_string_t msg, fa_string_t origin);

void fa_thread_fatal(char *msg, int error)
{
    fa_log_error_from(fa_string_from_utf8(msg),
                      fa_string_dappend(fa_string("Doremir.Thread, error code: "),
                                        fa_string_format_integral("%d", error)));
    exit(error);
}

