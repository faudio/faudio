
#include <doremir/audio_engine.h>
#include <doremir/thread.h>
#include <doremir/util.h>

int  version[3] = { 2, 0, 0 };
char *bits      = sizeof(void*) == 4 ? "32-bit" : "64-bit";

doremir_closure_t* new_closure(doremir_unary_t function, doremir_ptr_t value)
{
    doremir_closure_t *r = malloc(sizeof(doremir_closure_t));
    r->function = function;
    r->value = value;
    return r;
}

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
    // doremir_closure_t r = { printer, (doremir_ptr_t) 10 };
    doremir_thread_t t = doremir_thread_create(new_closure(printer, (doremir_ptr_t) 10));

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


// bool foo_equal(doremir_ptr_t a, doremir_ptr_t b)
// {
//     return false;
// }
// 
// bool foo_less_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//     return false;
// }
// 
// bool foo_greater_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//     return false;
// }
// 
// doremir_ptr_t foo_impl(doremir_id_t interface)
// {
//     static doremir_equal_t  foo_equal_impl = { foo_equal };
//     static doremir_order_t foo_order_impl = { foo_less_than, foo_greater_than };
// 
//     switch (interface)
//     {
//     case doremir_equal:
//         return &foo_equal_impl;
// 
//     case doremir_order:
//         return &foo_order_impl;
// 
//     default:
//         return NULL;
//     }
// }             

// void test_list()
// {
//     list_t xs = lempty();
// }
     
void test_wrap()
{
    printf("bool: %s\n", doremir_type_str(fbool(true)));
    assert(tbool(fbool(true)) == true);
    assert(tbool(fbool(false)) == false);

    printf("int8: %s\n", doremir_type_str(fint8(62)));
    assert(tint8(fint8('h')) == 'h');
    assert(tint8(fint8(121)) == 121);
    assert(tint8(fint8(-42)) == -42);

    printf("int16: %s\n", doremir_type_str(fint16(12372)));
    printf("int16:%d\n", tint16(fint16(1267)));
    assert(tint16(fint16(1267)) == 1267);
    assert(tint16(fint16(-8712)) == -8712);

    printf("int32: %s\n", doremir_type_str(fint32(12372)));
    printf("int32:%d\n", tint32(fint32(1267)));
    assert(tint32(fint32(2147483646)) == 2147483646);
    assert(tint32(fint32(-343646748)) == -343646748);

    printf("int64: %s\n", doremir_type_str(fint64(12372)));
    printf("int64: %lli\n", tint64(fint64(9223372036854775807ll)));
    assert(tint64(fint64(4872837827878787871ll)) == 4872837827878787871ll);
    assert(tint64(fint64(-6888881236767676711ll)) == -6888881236767676711ll);

    printf("double: %s\n", doremir_type_str(fdouble(12372)));
    printf("double: %f\n", tdouble(fdouble(3.141592653589793)));
    assert(tdouble(fdouble(3.141592653589793)) == 3.141592653589793);
    assert(tdouble(fdouble(-1.4142135623730951)) == -1.4142135623730951);
}

void test_generic()
{
    printf("2    *  3.2   = %f\n", tdouble(doremir_multiply(fdouble(2), fdouble(3.2))));
    printf("1    /  3     = %f\n", tdouble(doremir_divide(fdouble(1), fdouble(3))));
    printf("1    +  1.5   = %f\n", tdouble(doremir_add(fdouble(1), fdouble(1.5))));

    
    printf("32                  + 1  = %i\n",   tint8(doremir_add(fint8(32), fint8(1))));
    printf("5123                + 1  = %i\n",   tint16(doremir_add(fint16(5123), fint16(1))));
    printf("2147483646          + 1  = %i\n",   tint32(doremir_add(fint32(2147483646), fint32(1))));
    printf("4872837827878787871 + 1  = %lli\n", tint64(doremir_add(fint64(4872837827878787871ll), fint64(1))));
    printf("32                  - 1  = %i\n",   tint8(doremir_subtract(fint8(32), fint8(1))));
    printf("5123                - 1  = %i\n",   tint16(doremir_subtract(fint16(5123), fint16(1))));
    printf("2147483646          - 1  = %i\n",   tint32(doremir_subtract(fint32(2147483646), fint32(1))));
    printf("4872837827878787871 - 1  = %lli\n", tint64(doremir_subtract(fint64(4872837827878787871ll), fint64(1))));
    printf("3                   / 2  = %i\n",   tint8(doremir_divide(fint8(33), fint8(2))));
    printf("3333                / 2  = %i\n",   tint16(doremir_divide(fint16(3333), fint16(2))));
    printf("3333333333          / 2  = %i\n",   tint32(doremir_divide(fint32(3333333333l), fint32(2))));
    printf("3333333333333333333 / 2  = %lli\n", tint64(doremir_divide(fint64(3333333333333333333ll), fint64(2))));

    printf("3                   / 1  = %i\n", tint8(doremir_divide(fint8(32), fint8(1))));

    printf("true == false = %s\n", (doremir_equal(fbool(true), fbool(true))) ? "true" : false);
    printf("32   == 32    = %s\n", (doremir_equal(fint8(32), fint8(32))) ? "true" : false);
    printf("5123 == 5123  = %s\n", (doremir_equal(fint16(5123), fint16(5123))) ? "true" : false);


}

void test_list()
{   
    {
        list_t xs = list(3,  fint16(1),fint16(2),fint16(3));
        list_t ys = doremir_list_copy(xs);
        printf("length: %d\n", doremir_list_length(xs));
        printf("length: %d\n", doremir_list_length(ys));
        printf("xs == ys: %d\n", doremir_equal(xs, ys));    
        // TODO destroy wrapped values
        doremir_destroy(xs);
        doremir_destroy(ys);
    }                       
    
    {
        list_t  xs = list(3, fint32(1),fint32(2),fint32(3));
        int32_t z  = tint32(doremir_list_sum(xs));
        int32_t p  = tint32(doremir_list_product(xs));
        int32_t m  = tint32(doremir_list_minimum(xs));
        int32_t n  = tint32(doremir_list_maximum(xs));
        printf("sum:  %d\n", z);
        printf("prod: %d\n", p);
        printf("min:  %d\n", m);
        printf("max:  %d\n", n);
    }
    
}  

void test_string()
{
    // char cs[] = {'h',0xc3,0xb6,'g','s',0};
    string_t s = doremir_string_from_utf8("högste");
    printf("len: %d\n", doremir_string_length(s));   // FIXME
    printf("str: %s\n", doremir_string_to_utf8(s));  // FIXME
}

int main (int argc, char const *argv[])
{
  printf("DoReMIR Audio engine %s v%d.%d.%d\n", bits, version[0], version[1], version[2]);
  printf("sizeof(doremir_ptr_t) = %d\n", (unsigned int) sizeof(doremir_ptr_t));
  printf("sizeof(int32_t) = %d\n", (unsigned int) sizeof(int32_t));
  printf("sizeof(int64_t) = %d\n", (unsigned int) sizeof(int64_t));
  printf("sizeof(wchar_t) = %d\n", (unsigned int) sizeof(wchar_t));
  // printf("sizeof(uint32_t) = %d\n", (unsigned int) sizeof(uint32_t));
  // printf("sizeof(void*) = %d\n", (unsigned int) sizeof(void*));

  {
      doremir_audio_engine_initialize();
      // int c = getopt(argc, (char**) argv, "abc:");
      // iconv_t cd = iconv_open("WCHAR_T", "UTF-8");

      // test_thread();
      // test_mutex();
      // test_cond();
      // test_wrap();
      test_generic();
      test_list();
      test_string();

      doremir_audio_engine_terminate();
  }
  return 0;
}
