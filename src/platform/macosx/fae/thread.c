
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/thread.h>
#include <fae/util.h>

#include <pthread.h>
#include <unistd.h>


struct _fae_thread_t {
    impl_t                  impl;       //  Interface dispatcher
    pthread_t               native;
};

struct _fae_thread_mutex_t {
    impl_t                  impl;       //  Interface dispatcher
    pthread_mutex_t         native;
};

// struct _fae_thread_condition_t {
//     impl_t                  impl;       //  Interface dispatcher
//     pthread_cond_t          native;
//     fae_thread_mutex_t  mutex;
// };

static pthread_t main_thread_g = NULL;

static void fae_thread_fatal(char *msg, int error);
ptr_t thread_impl(fae_id_t interface);
ptr_t mutex_impl(fae_id_t interface);
// ptr_t condition_impl(fae_id_t interface);

// --------------------------------------------------------------------------------

void fae_thread_initialize()
{
    main_thread_g = pthread_self();
}

void fae_thread_terminate()
{
    main_thread_g = NULL;
}

inline static thread_t new_thread()
{
    fae_thread_t thread = fae_new(thread);
    thread->impl = &thread_impl;
    return thread;
}
inline static void delete_thread(thread_t thread)
{
    fae_delete(thread);
}

// --------------------------------------------------------------------------------


/** Create a new thread executing the given function asynhronously.

    Threads have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @param func Function to execute.
    @param data Value to be passed to the function.
    @return     A new thread executing concurrently with the current thread.
 */
fae_thread_t fae_thread_create(fae_nullary_t func, fae_ptr_t data)
{
    fae_thread_t thread = new_thread();

    int result = pthread_create(&thread->native, NULL, func, data);

    if (result != 0) {
        fae_thread_fatal("create", result);
    }

    return thread;
}

/** Sleep the current thread for the given time.
 */
void fae_thread_sleep(fae_thread_millis_t s)
{
    usleep(s * 1000);
}

/** Destroy a thread, and return after its associated function has returned.
    @param thread Thread to join (destroyed).
  */
void fae_thread_join(fae_thread_t thread)
{
    int result = pthread_join(thread->native, NULL);
    fae_delete(thread);

    if (result != 0) {
        fae_thread_fatal("join", result);
    }
}

/** Destroy a thread and return directly. The associated function may continous executing
    in the background.
    @param thread Thread to detach (destroyed).
  */
void fae_thread_detach(fae_thread_t thread)
{
    int result = pthread_detach(thread->native);
    fae_delete(thread);

    if (result != 0) {
        fae_thread_fatal("detach", result);
    }
}

/** Return the main thread.
  */
fae_thread_t fae_thread_main()
{
    assert(main_thread_g && "Module not initialized");

    fae_thread_t thread = new_thread();
    thread->native = main_thread_g;
    return thread;
}

/** Return the current thread.
  */
fae_thread_t fae_thread_current()
{
    fae_thread_t thread = new_thread();
    thread->native = pthread_self();
    return thread;
}


// --------------------------------------------------------------------------------

/** Create a mutex.

    Mutexes have single-ownership semantics and must be finalized by passing it
    to a destructive function.
 */
fae_thread_mutex_t fae_thread_create_mutex()
{
    fae_thread_mutex_t mutex = fae_new(thread_mutex);
    mutex->impl = &mutex_impl;

    int result = pthread_mutex_init(&mutex->native, NULL);

    if (result != 0) {
        fae_thread_fatal("create_mutex", result);
    }

    return mutex;
}

/** Destroy a mutex.
 */
void fae_thread_destroy_mutex(fae_thread_mutex_t mutex)
{
    int result = pthread_mutex_destroy(&mutex->native);
    fae_delete(mutex);

    if (result != 0) {
        fae_thread_fatal("destroy_mutex", result);
    }
}

/** Acquire the lock of a mutex.
 */
bool fae_thread_lock(fae_thread_mutex_t mutex)
{
    int result = pthread_mutex_lock(&mutex->native);

    if (result == 0) {
        return true;
    } else {
        fae_thread_fatal("unlock", result);
        assert(false && "Not reached");
    }
}

/** Try acquiring the lock of a mutex.
 */
bool fae_thread_try_lock(fae_thread_mutex_t mutex)
{
    int result = pthread_mutex_trylock(&mutex->native);

    switch (result) {
    case 0:
        return true;

    case EBUSY:
        return false;

    default:
        fae_thread_fatal("try_lock", result);
        assert(false && "Not reached");
    }
}

/** Release the lock of a mutex.
 */
bool fae_thread_unlock(fae_thread_mutex_t mutex)
{
    int result = pthread_mutex_unlock(&mutex->native);

    if (result == 0) {
        return true;
    } else {
        fae_thread_fatal("unlock", result);
        assert(false && "Not reached");
    }
}


// --------------------------------------------------------------------------------

// /** Create a condition.
//
//     Conditions have single-ownership semantics and must be finalized by passing it
//     to a destructive function.
//  */ fae_thread_condition_t fae_thread_create_condition(fae_thread_mutex_t mutex)
// {
//     fae_thread_condition_t cond = fae_new(thread_condition);
//     cond->impl = &condition_impl;
//     cond->mutex = mutex;
//
//     int result = pthread_cond_init(&cond->native, NULL);
//
//     if (result != 0) {
//         fae_thread_fatal("create_condition", result);
//     }
//
//     return cond;
// }
//
// /** Destroy a condition.
//  */
// void fae_thread_destroy_condition(fae_thread_condition_t cond)
// {
//     int result = pthread_cond_destroy(&cond->native);
//     fae_delete(cond);
//
//     if (result != 0) {
//         fae_thread_fatal("destroy_condition", result);
//     }
// }
//
// /** Wait for a condition to be signaled.
//  */
// void fae_thread_wait_for(fae_thread_condition_t cond)
// {
//     int result = pthread_cond_wait(&cond->native, &cond->mutex->native);
//
//     if (result != 0) {
//         fae_thread_fatal("wait_for", result);
//     }
// }
//
// /** Signal a condition to one listener.
//  */
// void fae_thread_notify(fae_thread_condition_t cond)
// {
//     int result = pthread_cond_signal(&cond->native);
//
//     if (result != 0) {
//         fae_thread_fatal("notify", result);
//     }
// }
//
// /** Signal a condition to all listeners.
//  */
// void fae_thread_notify_all(fae_thread_condition_t cond)
// {
//     int result = pthread_cond_broadcast(&cond->native);
//
//     if (result != 0) {
//         fae_thread_fatal("notify_all", result);
//     }
// }
//

// --------------------------------------------------------------------------------

bool thread_equal(ptr_t m, ptr_t n)
{
    thread_t x = (thread_t) m;
    thread_t y = (thread_t) n;

    return pthread_equal(x->native, y->native);
}

bool thread_less_than(ptr_t m, ptr_t n)
{
    thread_t x = (thread_t) m;
    thread_t y = (thread_t) n;
    return x->native < y->native;
}

bool thread_greater_than(ptr_t m, ptr_t n)
{
    thread_t x = (thread_t) m;
    thread_t y = (thread_t) n;
    return x->native > y->native;
}

fae_string_t thread_show(ptr_t a)
{
    thread_t x = (thread_t) a;

    string_t str = string("<Thread ");
    str = string_dappend(str, fae_string_format_integral(" %p", (long) x->native));
    str = string_dappend(str, string(">"));
    return str;
}

ptr_t thread_impl(fae_id_t interface)
{
    static fae_equal_t thread_equal_impl
        = { thread_equal };
    static fae_order_t thread_order_impl
        = { thread_less_than, thread_greater_than };
    static fae_string_show_t thread_show_impl
        = { thread_show };

    switch (interface) {
    case fae_equal_i:
        return &thread_equal_impl;

    case fae_order_i:
        return &thread_order_impl;

    case fae_string_show_i:
        return &thread_show_impl;

    default:
        return NULL;
    }
}

fae_string_t mutex_show(ptr_t a)
{
    string_t str = string("<Mutex ");
    str = string_dappend(str, fae_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void mutex_destroy(ptr_t a)
{
    fae_thread_unlock(a);
}

ptr_t mutex_impl(fae_id_t interface)
{
    static fae_string_show_t mutex_show_impl
        = { mutex_show };
    static fae_destroy_t mutex_destroy_impl
        = { mutex_destroy };

    switch (interface) {
    case fae_string_show_i:
        return &mutex_show_impl;

    case fae_destroy_i:
        return &mutex_destroy_impl;

    default:
        return NULL;
    }
}

// fae_string_t condition_show(ptr_t a)
// {
//     string_t str = string("<Condition ");
//     str = string_dappend(str, fae_string_format_integral(" %p", (long) a));
//     str = string_dappend(str, string(">"));
//     return str;
// }
//
// void condition_destroy(ptr_t a)
// {
//     fae_thread_destroy_condition(a);
// }
//
// ptr_t condition_impl(fae_id_t interface)
// {
//     static fae_string_show_t condition_show_impl
//         = { condition_show };
//     static fae_destroy_t condition_destroy_impl
//         = { condition_destroy };
//
//     switch (interface) {
//     case fae_string_show_i:
//         return &condition_show_impl;
//
//     case fae_destroy_i:
//         return &condition_destroy_impl;
//
//     default:
//         return NULL;
//     }
// }


// --------------------------------------------------------------------------------

void fae_audio_engine_log_error_from(fae_string_t msg, fae_string_t origin);

void fae_thread_fatal(char *msg, int error)
{
    fae_audio_engine_log_error_from(string(msg), string("Doremir.Thread"));
    exit(error);
}

