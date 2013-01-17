
#include <doremir/audio_engine.h>
#include <doremir/priority_queue.h>
#include <doremir/thread.h>
#include <doremir/util.h>

int  version[3] = { 2, 0, 0 };
char *bits      = sizeof(void*) == 4 ? "32-bit" : "64-bit";

void test_section()
{
    printf("\n\n--------------------\n");
}


void test_wrap()
{
    test_section();
    // FIXME leaks

    printf("bool:       %s\n", doremir_type_str(b(true)));
    assert(tb(b(true)) == true);
    assert(tb(b(false)) == false);

    printf("int8:       %s\n", doremir_type_str(i8(62)));
    assert(ti8(i8('h')) == 'h');
    assert(ti8(i8(121)) == 121);
    assert(ti8(i8(-42)) == -42);

    printf("int16:      %s\n", doremir_type_str(i16(12372)));
    printf("int16:      %d\n", ti16(i16(1267)));
    assert(ti16(i16(1267)) == 1267);
    assert(ti16(i16(-8712)) == -8712);

    printf("int32:      %s\n", doremir_type_str(i32(12372)));
    printf("int32:      %d\n", ti32(i32(1267)));
    assert(ti32(i32(2147483646)) == 2147483646);
    assert(ti32(i32(-343646748)) == -343646748);

    printf("int64:      %s\n", doremir_type_str(i64(12372)));
    printf("int64:      %lli\n", ti64(i64(9223372036854775807ll)));
    assert(ti64(i64(4872837827878787871ll)) == 4872837827878787871ll);
    assert(ti64(i64(-6888881236767676711ll)) == -6888881236767676711ll);

    printf("double:     %s\n", doremir_type_str(d(12372)));
    printf("double:     %f\n", td(d(3.141592653589793)));
    assert(td(d(3.141592653589793)) == 3.141592653589793);
    assert(td(d(-1.4142135623730951)) == -1.4142135623730951);
}

void test_generic()
{
    test_section();
    // TODO leaks

    printf("2 * 3.2                      ==> %f\n",   td(doremir_multiply(d(2), d(3.2))));
    printf("1 / 3                        ==> %f\n",   td(doremir_divide(d(1), d(3))));
    printf("1 + 1.5                      ==> %f\n",   td(doremir_add(d(1), d(1.5))));

    printf("32                  + 1      ==> %i\n",   ti8(doremir_add(i8(32), i8(1))));
    printf("5123                + 1      ==> %i\n",   ti16(doremir_add(i16(5123), i16(1))));
    printf("2147483646          + 1      ==> %i\n",   ti32(doremir_add(i32(2147483646), i32(1))));
    printf("4872837827878787871 + 1      ==> %lli\n", ti64(doremir_add(i64(4872837827878787871ll), i64(1))));
    printf("32                  - 1      ==> %i\n",   ti8(doremir_subtract(i8(32), i8(1))));
    printf("5123                - 1      ==> %i\n",   ti16(doremir_subtract(i16(5123), i16(1))));
    printf("2147483646          - 1      ==> %i\n",   ti32(doremir_subtract(i32(2147483646), i32(1))));
    printf("4872837827878787871 - 1      ==> %lli\n", ti64(doremir_subtract(i64(4872837827878787871ll), i64(1))));
    printf("3                   / 2      ==> %i\n",   ti8(doremir_divide(i8(33), i8(2))));
    printf("3333                / 2      ==> %i\n",   ti16(doremir_divide(i16(3333), i16(2))));
    printf("3333333333          / 2      ==> %i\n",   ti32(doremir_divide(i32(3333333333l), i32(2))));
    printf("3333333333333333333 / 2      ==> %lli\n", ti64(doremir_divide(i64(3333333333333333333ll), i64(2))));
    printf("3                   / 1      ==> %i\n",   ti8(doremir_divide(i8(32), i8(1))));

    printf("true == false                ==> %s\n",   (doremir_equal(b(true), b(true))) ? "true" : false);
    printf("32   == 32                   ==> %s\n",   (doremir_equal(i8(32), i8(32))) ? "true" : false);
    printf("5123 == 5123                 ==> %s\n",   (doremir_equal(i16(5123), i16(5123))) ? "true" : false);


}


static inline void memdump(void* s, size_t n)
{
    for (size_t i = 0; i < n; ++i)
        printf("%x ", *((unsigned char*) (s + i)) );
    printf("\n");
}

void test_string()
{
    test_section();
    {
        string_t s = doremir_string_single('v');
        doremir_print("str: %s\n", s);
        doremir_destroy(s);
    }

    {
        // char* cs = " 新隶体 "; // length 5
        char* cs = "höglund";

        string_t s = string(cs);
        printf("len: %i\n", doremir_string_length(s));
        doremir_print("str: %s\n", s);

        printf("charAt 0: %x\n", char_at(0,s));
        printf("charAt 1: %x\n", char_at(1,s));
        printf("charAt 2: %x\n", char_at(2,s));
        doremir_destroy(s);
    }

    {
        string_t s = string("foo");
        string_t t = string("bar");
        string_t u = string_append(s, t);
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
            string_t u = string_dappend(s, t);
            doremir_print("str: %s\n", u);
            doremir_destroy(u);
        }
    }

}

void test_show()
{
    test_section();
    doremir_print("\n", NULL);
    doremir_print("%s\n", b(0));
    doremir_print("%s\n", i8(129));
    doremir_print("%s\n", i16(129));
    doremir_print("%s\n", i32(64000));
    doremir_print("%s\n", d(3.1415));
    doremir_print("%s\n", empty());
    doremir_dprint("%s\n", list(i8(1)));
    doremir_dprint("%s\n", list(i8(1), i8(2), list(i8(1), i8(2), b(true))));
    doremir_dprint("%s\n", list(
        pair(string("hans"), string("höglund")),
        pair(string("lisa"), string("streich")),
        pair(string("mats"), string("erlandsson"))
    ));
}

void test_compare()
{
    test_section();
    doremir_print("\"abc\" <  \"abd\"               ==> %s\n", b(doremir_less_than(string("abc"), string("abd"))));
    doremir_print("\"abc\" <= \"abd\"               ==> %s\n", b(doremir_less_than_equal(string("abc"), string("abd"))));
    doremir_print("\"abc\" >  \"abd\"               ==> %s\n", b(doremir_greater_than(string("abc"), string("abd"))));
    doremir_print("\"abc\" >= \"abd\"               ==> %s\n", b(doremir_less_than_equal(string("abc"), string("abd"))));
}

void test_rational()
{
    test_section();
    doremir_print("1/3 <  1/2                   ==> %s\n", b(doremir_less_than(ratio(1,3), ratio(1,2))));
    doremir_print("1/3 >  1/2                   ==> %s\n", b(doremir_greater_than(ratio(1,3), ratio(1,2))));
    doremir_print("1/3 == 2/6                   ==> %s\n", b(doremir_equal(ratio(1,3), ratio(2,6))));
    doremir_print("1/3 == 254/762               ==> %s\n", b(doremir_equal(ratio(1,3), ratio(254,762))));
    doremir_print("1/3 <= 7/8                   ==> %s\n", b(doremir_equal(ratio(1,3), ratio(254,762))));
}

void test_buffer()
{
    test_section();

    {
        doremir_buffer_t b = doremir_buffer_create(16);

        doremir_print("b                            ==> %s\n", b);
        for(int i = 0; i < 16; ++i) doremir_buffer_poke(b, i, i);
        doremir_print("b                            ==> %s\n", b);
        for(int i = 0; i < 16; ++i) doremir_buffer_poke(b, i, 0xff);
        doremir_print("b                            ==> %s\n", b);

        doremir_print("size(b)                      ==> %s\n", i32(doremir_buffer_size(b)));
    }

    {
        doremir_buffer_t b = doremir_buffer_create(1024);

        doremir_print("b                            ==> %s\n", b);
        for(int i = 0; i < 1024; ++i) doremir_buffer_poke(b, i, i);
        doremir_print("b                            ==> %s\n", b);
        for(int i = 0; i < 1024; ++i) doremir_buffer_poke(b, i, 0xff);
        doremir_print("b                            ==> %s\n", b);

        doremir_print("size(b)                      ==> %s\n", i32(doremir_buffer_size(b)));
    }
}

void test_time()
{
    test_section();

    doremir_time_t t = doremir_time_create(1,0,0,ratio(25,8));
    doremir_time_t u = doremir_time_create(0,1,1,ratio(58,1));
    doremir_print("t                            ==> %s\n", t);
    doremir_print("u                            ==> %s\n", u);
    doremir_print("t + u                        ==> %s\n", doremir_add(t, u));

    doremir_print("doremir_time_to_iso(t)       ==> %s\n", doremir_time_to_iso(t));
    doremir_print("doremir_time_to_iso(u)       ==> %s\n", doremir_time_to_iso(u));
}

void test_midi()
{
    test_section();

    {
        doremir_midi_t m = doremir_midi_create_simple(0xa, 60, 127);
        doremir_print("m                            ==> %s\n", m);
    }

    {
        doremir_buffer_t b = doremir_buffer_create(32);
        for(int i = 0; i < 32; ++i) doremir_buffer_poke(b, i, i);

        doremir_midi_t m = doremir_midi_create_sysex(b);
        doremir_print("m                            ==> %s\n", m);
    }
}

void test_type()
{
    // FIXME
    test_section();

    doremir_print("type(uint8)                  ==> %s\n", type(uint8));
    doremir_print("size_of(1024,type(uint8))    ==> %s\n", i32(doremir_type_size_of(1024,type(uint8))));
    doremir_print("align_of(1024,type(uint8))   ==> %s\n", i32(doremir_type_align_of(type(uint8))));
    printf("\n");

    doremir_print("type(double)                 ==> %s\n", type(double));
    doremir_print("size_of(1024,type(double))   ==> %s\n", i32(doremir_type_size_of(1024,type(double))));
    doremir_print("align_of(1024,type(double))  ==> %s\n", i32(doremir_type_align_of(type(double))));
    printf("\n");

    type_t t = type_pair(type(uint8), type(double));
    doremir_print("t                            ==> %s\n", t);
    doremir_print("size_of(1024,t)              ==> %s\n", i32(doremir_type_size_of(1024,t)));
    doremir_print("align_of(1024,t)             ==> %s\n", i32(doremir_type_align_of(t)));
    printf("\n");

    type_t u = type_pair(type_vector(type(uint8), 10), type(double));
    doremir_print("u                            ==> %s\n", u);
    doremir_print("size_of(1024,u)              ==> %s\n", i32(doremir_type_size_of(1024,u)));
    doremir_print("align_of(1024,u)             ==> %s\n", i32(doremir_type_align_of(u)));
    printf("\n");

    type_t u2 = type_pair(type_frame(type(uint8)), type(double));
    doremir_print("u2                           ==> %s\n", u2);
    doremir_print("size_of(1024,u2)             ==> %s\n", i32(doremir_type_size_of(1024,u2)));
    doremir_print("align_of(1024,u2)            ==> %s\n", i32(doremir_type_align_of(u2)));
    printf("\n");

    type_t v = type_pair(type(uint8),type_pair(type(uint8),type_pair(type(uint8),
        type_pair(type(uint8),type_pair(type(uint8),type_pair(type(uint8),
            type_pair(type(uint8),type_pair(type(uint8),type_pair(type(uint8),
            type(uint8))))))))));

    doremir_print("v                            ==> %s\n", v);
    doremir_print("size_of(1024,v)              ==> %s\n", i32(doremir_type_size_of(1024,v)));
    doremir_print("align_of(1024,v)             ==> %s\n", i32(doremir_type_align_of(v)));
    printf("\n");
}


ptr_t add10(ptr_t x, ptr_t _) { return (ptr_t) ((int) x + 10); }
void test_atomic()
{
    test_section();

    // treat as integer
    {
        doremir_atomic_t a = doremir_atomic_create();
        doremir_print("a                            ==> %s\n", a);

        doremir_atomic_set(a, (ptr_t) 0x5);
        doremir_print("a                            ==> %s\n", a);

        doremir_atomic_modify(a, add10, 0);
        doremir_print("a                            ==> %s\n", a);

        doremir_atomic_add(a, (ptr_t) -0xf);
        doremir_print("a                            ==> %s\n", a);

        doremir_atomic_exchange(a, (ptr_t) 1, (ptr_t) 0xfe);
        doremir_print("a                            ==> %s\n", a); // fails, still 0

        doremir_atomic_exchange(a, (ptr_t) 0, (ptr_t) 0xff);
        doremir_print("a                            ==> %s\n", a); // now ff
    }
}

struct reader_args { doremir_atomic_queue_t queue; atomic_t active; };
doremir_ptr_t queue_reader(doremir_ptr_t x)
{
    struct reader_args* args = x;
    doremir_atomic_queue_t q = args->queue;
    atomic_t               a = args->active;
    ptr_t                  v;

    while(true)
    {
        if (!tb(doremir_atomic_get(a)))
            return v;
        if ((v = doremir_atomic_queue_read(q)))
            printf("         |- %5d    \n", ti32(v));

        doremir_thread_sleep(5);
    }
}
void test_atomic_queue(int iter, long sleepTime)
{
    test_section();
    {
        doremir_atomic_queue_t q = doremir_atomic_queue_create();

        struct reader_args args = { q, atomic() };
        doremir_atomic_set(args.active, b(true));

        thread_t t = doremir_thread_create(queue_reader, &args);

        doremir_print("q                            ==> %s\n", q);

        for(int i = 0; i < iter; ++i)
        {
            doremir_thread_sleep(i % 10 * sleepTime);
            doremir_atomic_queue_write(q, i32(i));
            printf("  %5d -|  \n", i);
        }

        doremir_thread_sleep(sleepTime);
        doremir_atomic_set(args.active, b(false));
        doremir_thread_join(t); // TODO how to kill?
        doremir_destroy(q);
    }
}

void test_atomic_stack(int iter, long sleepTime)
{
    test_section();
}

void test_atomic_ring_buffer()
{
    test_section();
}

ptr_t printer(ptr_t data)
{
    int n = 0;
    while (n < 100)
    {
        printf("%d\n", n);
        n = n + ((int) data);
        doremir_thread_sleep(100);
    }
    return 0;
}
void test_thread()
{
    doremir_thread_t t, t2;
    t  = doremir_thread_create(printer, (ptr_t) 10);
    t2 = doremir_thread_create(printer, (ptr_t) 11);

    doremir_thread_sleep(1000);
    doremir_thread_join(t);
    doremir_thread_join(t2);
}


typedef struct { doremir_thread_mutex_t mut; int val; } lock_index;
ptr_t locker(ptr_t x)
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
        doremir_thread_t t = doremir_thread_create(locker, (ptr_t) &i);
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
ptr_t receiver(ptr_t x)
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

    doremir_thread_t s = doremir_thread_create(sender, (doremir_ptr_t) &h);
    doremir_thread_t r = doremir_thread_create(receiver, (doremir_ptr_t) &h);

    doremir_thread_join(s);
    doremir_thread_detach(r);
}
     


void test_for_each()
{
    doremir_let(x, 33)
    {
        doremir_let(y, 1)
            doremir_let(z, x + y)
                doremir_print("%s\n", i32(z));
    }

    doremir_with(list, list(i32(1),i32(2),i32(3),i32(4)), doremir_destroy(list))
    {
        doremir_for_each(x, list)
        {
            doremir_print(">    %s\n", x);
        }
    }
}

bool is_even16(ptr_t data, ptr_t p)
{
    return ti16(p) % 2 == 0;
}
bool is_odd16(ptr_t data, ptr_t p)
{
    return ti16(p) % 2 != 0;
}
ptr_t times2(ptr_t data, ptr_t p)
{
    return i16(2 * ti16(p));
}
ptr_t times10(ptr_t data, ptr_t p)
{
    return i16(10 * ti16(p));
}
ptr_t call1(ptr_t f, ptr_t x)
{
    nullary_t g = f;
    return g(x);
}
ptr_t call2(ptr_t f, ptr_t x, ptr_t y)
{
    unary_t g = f;
    return g(x, y);
}
void test_list()
{
    test_section();

    {
        printf("\n");

        list_t as = empty();
        doremir_print("empty()                      ==> %s\n", as);
        doremir_destroy(as);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3));
        list_t bs = doremir_list_cons(i16(0),as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("cons(0,as)                   ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3));
        list_t bs = doremir_list_append(as,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("append(as,as)                ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3));
        list_t bs = doremir_list_copy(as);
        doremir_print("as                           ==> %s\n", as);
        doremir_print("copy(as)                     ==> %s\n", bs);
        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3));
        list_t bs = doremir_list_init(as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("init(as)                     ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3));
        ptr_t v = doremir_list_last(as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("last(as)                     ==> %s\n", v);

        doremir_destroy(as);
        doremir_destroy(v);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3));

        doremir_print("as                           ==> %s\n", as);
        doremir_print("length(as)                   ==> %s\n", i16(doremir_list_length(as)));

        doremir_destroy(as);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_reverse(as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("reverse(as)                  ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }

    // TODO sort

    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_take(3,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("take(3,as)                   ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_drop(3,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("drop(3,as)                   ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        ptr_t v = doremir_list_index(1,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("index(1,as)                  ==> %s\n", v);

        doremir_destroy(as);
    }

    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_range(1,3,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("range(1,3,as)                ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_remove_range(1,3,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("removeRange(1,3,as)          ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }

    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t xs = list(i16(0),i16(0));
        list_t bs = doremir_list_insert_range(2,xs,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("insertRange(2,list(0,0),as)  ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(xs);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_insert(2,i16(0),as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("insert(2,0,as)               ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_insert(0,i16(0),as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("insert(0,1,as)               ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t bs = doremir_list_remove(2,as);

        doremir_print("as                           ==> %s\n", as);
        doremir_print("remove(2,as)                 ==> %s\n", bs);

        doremir_destroy(as);
        doremir_destroy(bs);
    }

    // has
    // find
    // findIndex

    {
        printf("\n");

        list_t as = list(i16(1),i16(3),i16(5));

        doremir_print("as                           ==> %s\n", as);
        doremir_print("indexOf(0,as)                ==> %s\n", i16(doremir_list_index_of(i16(0),as)));
        doremir_print("indexOf(1,as)                ==> %s\n", i16(doremir_list_index_of(i16(1),as)));
        doremir_print("indexOf(2,as)                ==> %s\n", i16(doremir_list_index_of(i16(2),as)));
        doremir_print("indexOf(3,as)                ==> %s\n", i16(doremir_list_index_of(i16(3),as)));
        doremir_print("indexOf(4,as)                ==> %s\n", i16(doremir_list_index_of(i16(4),as)));
        doremir_print("indexOf(5,as)                ==> %s\n", i16(doremir_list_index_of(i16(5),as)));

        doremir_destroy(as);
    }
    {
        printf("\n");

        list_t xs = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t ys = doremir_list_filter(is_odd16, 0, xs);

        doremir_print("xs                           ==> %s\n", xs);
        doremir_print("filter(is_odd,ys)            ==> %s\n", ys);

        doremir_destroy(xs);
        doremir_destroy(ys);
    }
    {
        printf("\n");

        list_t xs = list(i16(1),i16(2),i16(3),i16(4),i16(5));
        list_t ys = doremir_list_map(times10, 0,xs);

        doremir_print("xs                           ==> %s\n", xs);
        doremir_print("map(times10,ys)              ==> %s\n", ys);

        doremir_destroy(xs);
        doremir_destroy(ys);
    }

    // concatMap

    {
        printf("\n");

        list_t xs = range(0,50000);

        xs = doremir_list_dreverse(xs);
        // doremir_print("reverse(xs)                  ==> %s\n", xs);

        xs = doremir_list_dmap(call1, i16, xs);
        // doremir_print("xs                           ==> %s\n", xs);

        xs = doremir_list_dfilter(is_odd16, 0, xs);
        // doremir_print("filter(is_odd,xs)            ==> %s\n", xs);

        xs = doremir_list_dmap(times10, 0, xs);
        // doremir_print("map(times10, xs)             ==> %s\n", xs);

        doremir_destroy(xs);
    }
    {
        printf("\n");

        list_t xs = range(0,12);
        xs = doremir_list_dmap(call1, i8, xs);

        doremir_print("xs                           ==> %s\n", xs);
        ptr_t sum = doremir_list_dfold_left(call2, doremir_add, i8(0), xs);
        doremir_print("sum(xs)                      ==> %s\n", sum);
    }
}

void test_set()
{
    test_section();
    {
        printf("\n");

        set_t a = set(i16(1),i16(3),i16(2));

        a = doremir_set_dadd(i16(1), a);
        a = doremir_set_dadd(i16(5), a);
        a = doremir_set_dadd(i16(3), a);
        a = doremir_set_dremove(i16(3), a);

        doremir_print ("a                            ==> %s\n", a);
        doremir_print ("size(a)                      ==> %s\n", i16(doremir_set_size(a)));
        doremir_destroy(a);
    }

    {
        printf("\n");

        set_t a = set(i16(1),i16(2),i16(3));
        set_t b = set(i16(3),i16(4));

        doremir_print ("a                            ==> %s\n", a);
        doremir_print ("b                            ==> %s\n", b);
        doremir_dprint("a + b                        ==> %s\n", doremir_set_sum(a, b));

        doremir_destroy(a);
        doremir_destroy(b);
    }

    {
        printf("\n");

        set_t a = set(i16(1),i16(2),i16(3));
        set_t b = set(i16(3),i16(4));

        doremir_print ("a                            ==> %s\n", a);
        doremir_print ("b                            ==> %s\n", b);
        doremir_dprint("a - b                        ==> %s\n", doremir_set_difference(a, b));

        doremir_destroy(a);
        doremir_destroy(b);
    }

    {
        printf("\n");

        set_t a = set(i16(1),i16(2),i16(3));
        set_t b = set(i16(3),i16(4));

        doremir_print ("a                            ==> %s\n", a);
        doremir_print ("b                            ==> %s\n", b);
        doremir_dprint("a x b                        ==> %s\n", doremir_set_product(a, b));

        doremir_destroy(a);
        doremir_destroy(b);
    }

    {
        printf("\n");

        set_t a = set(string("foo"), string("bar"));
        set_t b = set(string("hi"), string("ho"));
        set_t c = set(i16(0),i16(1));

        doremir_print ("a                            ==> %s\n", a);
        doremir_print ("b                            ==> %s\n", b);
        doremir_dprint("a x b                        ==> %s\n", doremir_set_product(a, b));

        set_t ab = doremir_set_product(a, b);
        doremir_dprint("a x b x c                    ==> %s\n", doremir_set_product(ab, c));
        doremir_destroy(ab);

        doremir_destroy(a);
        doremir_destroy(b);
        doremir_destroy(c);
    }
}    

void test_map()
{
    test_section();
    {
        printf("\n");

        map_t a = doremir_map_empty();
        a = doremir_map_add(string("name"), string("Hans"), a);
        a = doremir_map_add(string("age"), i16(22), a);
        // a = doremir_map_remove(string("age"), a);
        // a = doremir_map_add(string("age"), i16(25), a);
        a = doremir_map_add(string("skills"), list(string("programming"), string("composition")), a);

        // a = doremir_map_add(string("notes"), doremir_midi_create_simple(note_on, 60, 127), a);
        // a = doremir_map_add(string("types"), type_pair(type(uint8),type(uint8)), a);
        a = doremir_map_add(string("happy"), b(true), a);
        a = doremir_map_add(string("pair"), pair(b(true), d(3.1415)), a);

        a = doremir_map_add(string("ratio"), ratio(1, 3), a);
        a = doremir_map_add(string("ratio2"), doremir_multiply(ratio(4, 4444), ratio(1,2)), a);

        // a = doremir_map_dadd(i16(1), a);
        // a = doremir_map_dadd(i16(5), a);
        // a = doremir_map_dadd(i16(3), a);
        // a = doremir_map_dremove(i16(3), a);

        doremir_print ("a                            ==> %s\n", a);
        doremir_print ("size(a)                      ==> %s\n", i16(doremir_map_size(a)));
        doremir_destroy(a);
    }   
}

void test_priority_queue()
{
    test_section();

    priority_queue_t q = doremir_priority_queue_empty();
    srand(time(NULL));

    for (int i = 0; i < 1000; ++i)
        doremir_priority_queue_insert(doremir_add(hours(rand() % 24), seconds(rand() % 3600)), q);

    while(doremir_priority_queue_peek(q))
        doremir_dprint("     -> %s \n", doremir_priority_queue_pop(q));
    
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
      
      test_wrap();
      test_generic();
      test_string();
      test_show();
      test_compare();
      test_rational();
      test_buffer();
      test_time();
      test_midi();
      test_type();


      test_atomic();
      test_atomic_queue(5, 2);
      // test_atomic_queue(10, 10);
      // test_atomic_queue(300, 2);
      // test_atomic_stack(5, 2);
      test_atomic_ring_buffer();

      // test_thread();
      // test_mutex();
      // test_cond();
      // futures
      // improvings

      test_for_each();

      test_list();
      test_set();
      test_map();
      // error
      // json

      // processors

      // dispatchers
      test_priority_queue();
      // schedulers

      doremir_audio_engine_terminate();
  }
  return 0;
}
