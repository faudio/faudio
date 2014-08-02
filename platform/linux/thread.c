
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/thread.h>
#include <fa/util.h>

#include <pthread.h>
#include <unistd.h>


struct _fa_thread_t {
    fa_impl_t                  impl;       //  Interface dispatcher
    pthread_t               native;
};

struct _fa_thread_mutex_t {
    fa_impl_t                  impl;       //  Interface dispatcher
    pthread_mutex_t         native;
};

static pthread_t main_thread_g = NULL;

static void fa_thread_fatal(char *msg, int error);
fa_ptr_t thread_impl(fa_id_t interface);
fa_ptr_t mutex_impl(fa_id_t interface);

// --------------------------------------------------------------------------------

void fa_thread_initialize()
{
    main_thread_g = pthread_self();
}

void fa_thread_terminate()
{
    main_thread_g = NULL;
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


fa_thread_t fa_thread_create(fa_nullary_t func, fa_ptr_t data)
{
    fa_thread_t thread = new_thread();

    int result = pthread_create(&thread->native, NULL, func, data);

    if (result != 0) {
        fa_thread_fatal("create", result);
    }

    return thread;
}

void fa_thread_sleep(fa_time_milliseconds_t ms)
{
    usleep(ms * 1000);
}

void fa_thread_join(fa_thread_t thread)
{
    int result = pthread_join(thread->native, NULL);
    fa_delete(thread);

    if (result != 0) {
        fa_thread_fatal("join", result);
    }
}

void fa_thread_detach(fa_thread_t thread)
{
    int result = pthread_detach(thread->native);
    fa_delete(thread);

    if (result != 0) {
        fa_thread_fatal("detach", result);
    }
}

fa_thread_t fa_thread_main()
{
    assert(main_thread_g && "Module not initialized");

    fa_thread_t thread = new_thread();
    thread->native = main_thread_g;
    return thread;
}

fa_thread_t fa_thread_current()
{
    fa_thread_t thread = new_thread();
    thread->native = pthread_self();
    return thread;
}


// --------------------------------------------------------------------------------

fa_thread_mutex_t fa_thread_create_mutex()
{
    fa_thread_mutex_t mutex = fa_new(thread_mutex);
    mutex->impl = &mutex_impl;

    int result = pthread_mutex_init(&mutex->native, NULL);

    if (result != 0) {
        fa_thread_fatal("create_mutex", result);
    }

    return mutex;
}

void fa_thread_destroy_mutex(fa_thread_mutex_t mutex)
{
    int result = pthread_mutex_destroy(&mutex->native);
    fa_delete(mutex);

    if (result != 0) {
        fa_thread_fatal("destroy_mutex", result);
    }
}

bool fa_thread_lock(fa_thread_mutex_t mutex)
{
    int result = pthread_mutex_lock(&mutex->native);

    if (result == 0) {
        return true;
    } else {
        fa_thread_fatal("unlock", result);
        assert(false && "Not reached");
    }
}

bool fa_thread_try_lock(fa_thread_mutex_t mutex)
{
    int result = pthread_mutex_trylock(&mutex->native);

    switch (result) {
    case 0:
        return true;

    case EBUSY:
        return false;

    default:
        fa_thread_fatal("try_lock", result);
        assert(false && "Not reached");
    }
}

bool fa_thread_unlock(fa_thread_mutex_t mutex)
{
    int result = pthread_mutex_unlock(&mutex->native);

    if (result == 0) {
        return true;
    } else {
        fa_thread_fatal("unlock", result);
        assert(false && "Not reached");
    }
}

// --------------------------------------------------------------------------------

bool thread_equal(fa_ptr_t m, fa_ptr_t n)
{
    fa_thread_t x = (fa_thread_t) m;
    fa_thread_t y = (fa_thread_t) n;

    return pthread_equal(x->native, y->native);
}

bool thread_less_than(fa_ptr_t m, fa_ptr_t n)
{
    fa_thread_t x = (fa_thread_t) m;
    fa_thread_t y = (fa_thread_t) n;
    return x->native < y->native;
}

bool thread_greater_than(fa_ptr_t m, fa_ptr_t n)
{
    fa_thread_t x = (fa_thread_t) m;
    fa_thread_t y = (fa_thread_t) n;
    return x->native > y->native;
}

fa_string_t thread_show(fa_ptr_t a)
{
    fa_thread_t x = (fa_thread_t) a;

    fa_string_t str = fa_string("<Thread ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) x->native));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

fa_ptr_t thread_impl(fa_id_t interface)
{
    static fa_equal_t thread_equal_impl
        = { thread_equal };
    static fa_order_t thread_order_impl
        = { thread_less_than, thread_greater_than };
    static fa_string_show_t thread_show_impl
        = { thread_show };

    switch (interface) {
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
    fa_thread_unlock(a);
}

fa_ptr_t mutex_impl(fa_id_t interface)
{
    static fa_string_show_t mutex_show_impl
        = { mutex_show };
    static fa_destroy_t mutex_destroy_impl
        = { mutex_destroy };

    switch (interface) {
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
    fa_log_error_from(fa_string(msg), fa_string("Doremir.Thread"));
    exit(error);
}

