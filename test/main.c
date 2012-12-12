
#include <doremir/audio_engine.h>
#include <doremir/thread.h>

int version[3] = { 2, 0, 0 };


doremir_ptr_t printer(doremir_ptr_t x)
{
    int n = 0;
    while (n < 100)
    {
        printf("%d\n", n);
        n = n + (int) x;
        doremir_thread_sleep(100);
    }
    return 0;
}
void test_thread()
{                                               
    doremir_closure_t r = { printer, (doremir_ptr_t) 10 };
    doremir_thread_t t = doremir_thread_create(&r);

    doremir_closure_t r2 = { printer, (doremir_ptr_t) 11 };
    doremir_thread_t t2 = doremir_thread_create(&r2);

    doremir_thread_sleep(1000);
    doremir_thread_join(t);
    doremir_thread_join(t2);
}


typedef struct { doremir_thread_mutex_t mut; int val; } lock_index;
doremir_ptr_t locker(doremir_ptr_t x)
{
    lock_index *i = (lock_index*) x;

    doremir_thread_lock(i->mut);
    printf("Acquired lock in thread %d\n", i->val);
    doremir_thread_sleep(200);
    doremir_thread_unlock(i->mut);
    printf("Released lock in thread %d\n", i->val);
    
    return 0;
}
void test_mutex()
{   
    doremir_thread_mutex_t m = doremir_thread_create_mutex();
                               
    for (int j = 0; j < 10; ++j)
    {
        lock_index i = { m, j };
        doremir_closure_t r = { locker, (doremir_ptr_t) &i };
        doremir_thread_t t = doremir_thread_create(&r);
        doremir_thread_sleep(100);
        doremir_thread_detach(t);
    }   
    
    doremir_thread_sleep(1200);
}     



typedef struct { 
    doremir_thread_mutex_t mut;
    doremir_thread_condition_t cond; 
    char* msg;
    } send_hub;
doremir_ptr_t sender(doremir_ptr_t x)
{
    send_hub *h = (send_hub*) x;
    static char * const msgs[10] = {
        "Sur", "le", "pond", "d'Avignon", "on", "y", "danse", "tous", "en", "round"
    };

    for (int i = 0; i < 10; ++i)           
    {
        doremir_thread_lock(h->mut);
        h->msg = msgs[i];
        printf("Sending: %s\n", h->msg);
        doremir_thread_notify(h->cond);
        doremir_thread_unlock(h->mut);

        doremir_thread_sleep(100);        
    }

    return 0;
}
doremir_ptr_t receiver(doremir_ptr_t x)
{
    send_hub *h = (send_hub*) x;
    
    while (true)
    {
        doremir_thread_lock(h->mut);
        doremir_thread_wait_for(h->cond);
        printf("                        Received: %s\n", h->msg);
        doremir_thread_unlock(h->mut);        
    }

    return 0;
}
void test_cond()
{
    doremir_thread_mutex_t m = doremir_thread_create_mutex();
    doremir_thread_condition_t c = doremir_thread_create_condition(m);
    send_hub h = { m, c, 0 };

    doremir_closure_t sr = { sender, (doremir_ptr_t) &h };
    doremir_thread_t s = doremir_thread_create(&sr);
    doremir_closure_t rr = { receiver, (doremir_ptr_t) &h };
    doremir_thread_t r = doremir_thread_create(&rr);

    doremir_thread_join(s);
    doremir_thread_detach(r);
}


int main (int argc, char const *argv[])
{
  printf("DoReMIR Audio engine v%d.%d.%d\n", version[0], version[1], version[2]);
  printf("sizeof(doremir_ptr_t) = %d\n", (unsigned int) sizeof(doremir_ptr_t));
  printf("sizeof(int32_t) = %d\n", (unsigned int) sizeof(int32_t));
  printf("sizeof(uint32_t) = %d\n", (unsigned int) sizeof(uint32_t));
  printf("sizeof(void*) = %d\n", (unsigned int) sizeof(void*));
  
  // int c = getopt(argc, (char**) argv, "abc:");
  // iconv_t cd = iconv_open("WCHAR_T", "UTF-8");
  
  test_thread();
  test_mutex();
  test_cond();

  return 0;
}