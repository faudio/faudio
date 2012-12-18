
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

void test_wrap()
{
    // FIXME leaks

    printf("bool: %s\n", doremir_type_str(b(true)));
    assert(tb(b(true)) == true);
    assert(tb(b(false)) == false);

    printf("int8: %s\n", doremir_type_str(i8(62)));
    assert(ti8(i8('h')) == 'h');
    assert(ti8(i8(121)) == 121);
    assert(ti8(i8(-42)) == -42);

    printf("int16: %s\n", doremir_type_str(i16(12372)));
    printf("int16:%d\n", ti16(i16(1267)));
    assert(ti16(i16(1267)) == 1267);
    assert(ti16(i16(-8712)) == -8712);

    printf("int32: %s\n", doremir_type_str(i32(12372)));
    printf("int32:%d\n", ti32(i32(1267)));
    assert(ti32(i32(2147483646)) == 2147483646);
    assert(ti32(i32(-343646748)) == -343646748);

    printf("int64: %s\n", doremir_type_str(i64(12372)));
    printf("int64: %lli\n", ti64(i64(9223372036854775807ll)));
    assert(ti64(i64(4872837827878787871ll)) == 4872837827878787871ll);
    assert(ti64(i64(-6888881236767676711ll)) == -6888881236767676711ll);

    printf("double: %s\n", doremir_type_str(d(12372)));
    printf("double: %f\n", td(d(3.141592653589793)));
    assert(td(d(3.141592653589793)) == 3.141592653589793);
    assert(td(d(-1.4142135623730951)) == -1.4142135623730951);
}

void test_generic()
{
    // TODO leaks

    printf("2    *  3.2   = %f\n", td(doremir_multiply(d(2), d(3.2))));
    printf("1    /  3     = %f\n", td(doremir_divide(d(1), d(3))));
    printf("1    +  1.5   = %f\n", td(doremir_add(d(1), d(1.5))));


    printf("32                  + 1  = %i\n",   ti8(doremir_add(i8(32), i8(1))));
    printf("5123                + 1  = %i\n",   ti16(doremir_add(i16(5123), i16(1))));
    printf("2147483646          + 1  = %i\n",   ti32(doremir_add(i32(2147483646), i32(1))));
    printf("4872837827878787871 + 1  = %lli\n", ti64(doremir_add(i64(4872837827878787871ll), i64(1))));
    printf("32                  - 1  = %i\n",   ti8(doremir_subtract(i8(32), i8(1))));
    printf("5123                - 1  = %i\n",   ti16(doremir_subtract(i16(5123), i16(1))));
    printf("2147483646          - 1  = %i\n",   ti32(doremir_subtract(i32(2147483646), i32(1))));
    printf("4872837827878787871 - 1  = %lli\n", ti64(doremir_subtract(i64(4872837827878787871ll), i64(1))));
    printf("3                   / 2  = %i\n",   ti8(doremir_divide(i8(33), i8(2))));
    printf("3333                / 2  = %i\n",   ti16(doremir_divide(i16(3333), i16(2))));
    printf("3333333333          / 2  = %i\n",   ti32(doremir_divide(i32(3333333333l), i32(2))));
    printf("3333333333333333333 / 2  = %lli\n", ti64(doremir_divide(i64(3333333333333333333ll), i64(2))));

    printf("3                   / 1  = %i\n", ti8(doremir_divide(i8(32), i8(1))));

    printf("true == false = %s\n", (doremir_equal(b(true), b(true))) ? "true" : false);
    printf("32   == 32    = %s\n", (doremir_equal(i8(32), i8(32))) ? "true" : false);
    printf("5123 == 5123  = %s\n", (doremir_equal(i16(5123), i16(5123))) ? "true" : false);


}

void test_list()
{
    // TODO leaks
    {
        list_t xs = list(i16(1),i16(2),i16(3));
        list_t ys = doremir_copy(xs);
        printf("length: %d\n", doremir_list_length(xs));
        printf("length: %d\n", doremir_list_length(ys));
        printf("xs == ys: %d\n", doremir_equal(xs, ys));
        // TODO destroy wrapped values
        doremir_destroy(xs);
        doremir_destroy(ys);
    }

    {
        // list_t  xs = list(i32(1),i32(2),i32(3));
        // int32_t z  = ti32(doremir_list_sum(xs));
        // int32_t p  = ti32(doremir_list_product(xs));
        // int32_t m  = ti32(doremir_list_minimum(xs));
        // int32_t n  = ti32(doremir_list_maximum(xs));
        // printf("sum:  %d\n", z);
        // printf("prod: %d\n", p);
        // printf("min:  %d\n", m);
        // printf("max:  %d\n", n);
        // // TODO destroy wrapped values
        // doremir_destroy(xs);
    }

}

static inline void memdump(void* s, size_t n)
{
    for (size_t i = 0; i < n; ++i)
        printf("%x ", *((unsigned char*) (s + i)) );
    printf("\n");
}

void test_string()
{
    {
        string_t s = doremir_string_single('v');
        doremir_print("str: %s\n", s);
        doremir_destroy(s);
    }

    {
        // char* cs = " 新隶体 "; // length 5
        char* cs = "höglund";

        string_t s = string(cs);
        printf("len: %i\n", slength(s));
        doremir_print("str: %s\n", s);

        printf("charAt 0: %x\n", char_at(0,s));
        printf("charAt 1: %x\n", char_at(1,s));
        printf("charAt 2: %x\n", char_at(2,s));
        doremir_destroy(s);
    }

    {
        string_t s = string("foo");
        string_t t = string("bar");
        string_t u = sappend(s, t);
        doremir_print("str: %s\n", s);
        doremir_print("str: %s\n", t);
        doremir_print("str: %s\n", u);
        doremir_destroy(s);
        doremir_destroy(t);
        doremir_destroy(u);
    }

    {
        string_t s = string("foo");
        string_t t = string("bar");
        doremir_print("str: %s\n", s);
        doremir_print("str: %s\n", t);
        {
            string_t u = sdappend(s, t);
            doremir_print("str: %s\n", u);
            doremir_destroy(u);
        }
        doremir_destroy(t);
    }

}

void test_show()
{
    doremir_print("\n", NULL);
    doremir_print("%s\n", b(0));
    doremir_print("%s\n", i8(129));
    doremir_print("%s\n", i16(129));
    doremir_print("%s\n", i32(64000));
    doremir_print("%s\n", d(3.1415));
    doremir_print("%s\n", empty() );
    doremir_print("%s\n", list(i8(1)));
    doremir_print("%s\n", list(i8(1), i8(2), list(i8(1), i8(2), b(true))));
    doremir_print("%s\n", list(string("hans"),string("höglund")));
}     

void test_compare()
{
    doremir_print("abc <  abd == %s\n", b(doremir_less_than(string("abc"), string("abd"))));
    doremir_print("abc <= abd == %s\n", b(doremir_less_than_equal(string("abc"), string("abd"))));
    doremir_print("abc >  abd == %s\n", b(doremir_greater_than(string("abc"), string("abd"))));
    doremir_print("abc >= abd == %s\n", b(doremir_less_than_equal(string("abc"), string("abd"))));
}

int main (int argc, char const *argv[])
{
  printf("DoReMIR Audio engine %s v%d.%d.%d\n", bits, version[0], version[1], version[2]);
  printf("sizeof(doremir_ptr_t) = %d\n", (unsigned int) sizeof(doremir_ptr_t));
  printf("sizeof(int32_t) = %d\n", (unsigned int) sizeof(int32_t));
  printf("sizeof(int64_t) = %d\n", (unsigned int) sizeof(int64_t));
  printf("sizeof(wchar_t) = %d\n", (unsigned int) sizeof(wchar_t));

  // while(true)
  {
      doremir_audio_engine_initialize();

      // test_thread();
      // test_mutex();
      // test_cond();
      test_wrap();
      test_generic();
      test_list();
      test_string();
      test_show();
      test_compare();

      doremir_audio_engine_terminate();
  }
  return 0;
}
