
#include <Windows.h>
#include <doremir/thread.h>

struct _doremir_thread_t
{
};

struct _doremir_thread_mutex_t
{
};

struct _doremir_thread_condition_t
{
};

doremir_thread_t doremir_thread_create()
{
    doremir_thread_t t = malloc(sizeof(struct _doremir_thread_t));
    // TODO
    int r = 0;
    if (r != 0)
        ; // TODO fatal
    return t;
}

void doremir_thread_join(doremir_thread_t t)
{
    int r = 0;
    free(t);
    if (r != 0)
        ; // TODO fatal
}

void doremir_thread_detach(doremir_thread_t t)
{
    int r = 0;
    free(t);
    if (r != 0)
        ; // TODO fatal
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

doremir_thread_condition_t doremir_thread_create_condition()
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
