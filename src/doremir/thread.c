
#include <pthread.h>
#include <doremir/thread.h>

struct _doremir_thread_t
{
  pthread_t native;
};

struct _doremir_thread_mutex_t
{
  pthread_mutex_t native;
};

struct _doremir_thread_condition_t
{
  pthread_cond_t native;
};

/** Creates a new thread.
 */
doremir_thread_t doremir_thread_create_thread()
{
  doremir_thread_t t = malloc(sizeof(struct _doremir_thread_t));
  // TODO pass func
  int r = pthread_create(&t->native, NULL, NULL, NULL);
  if (r != 0)
    ; // TODO fatal
}

void doremir_thread_join(doremir_thread_t t)
{
  int r = pthread_join(t->native, NULL);
  if (r != 0)
    ; // TODO fatal
}

void doremir_thread_detach(doremir_thread_t t)
{
  int r = pthread_detach(t->native);
  if (r != 0)
    ; // TODO fatal
}

doremir_thread_mutex_t doremir_thread_create_mutex()
{
  doremir_thread_mutex_t m = malloc(sizeof(struct _doremir_thread_mutex_t));
  int r = pthread_mutex_init(&m->native, NULL);
  if (r != 0)
    ; // TODO fatal
}

void doremir_thread_destroy_mutex(doremir_thread_mutex_t t)
{           
  int r = pthread_mutex_destroy(&t->native);
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
}

void doremir_thread_wait_for(doremir_thread_condition_t c)
{
  int r = 0;
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

