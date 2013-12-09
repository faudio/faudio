
#include <fa/fa.h>
#include <fa/util.h>

void test_section(char *str)
{
    printf("\n\n--------------------\n");
    fa_fa_log_info(string_dappend(string("Running test: "), string(str)));
}


// --------------------------------------------------------------------------------

void test_value_references()
{
    extern char *fa_type_str(fa_ptr_t a);

    test_section("Value references");
    // FIXME leaks

    printf("bool:       %s\n", fa_type_str(fb(true)));
    assert(tb(fb(true)) == true);
    assert(tb(fb(false)) == false);

    printf("int8:       %s\n", fa_type_str(i8(62)));
    assert(ti8(i8('h')) == 'h');
    assert(ti8(i8(121)) == 121);
    assert(ti8(i8(-42)) == -42);

    printf("int16:      %s\n", fa_type_str(i16(12372)));
    printf("int16:      %d\n", ti16(i16(1267)));
    assert(ti16(i16(1267)) == 1267);
    assert(ti16(i16(-8712)) == -8712);

    printf("int32:      %s\n", fa_type_str(i32(12372)));
    printf("int32:      %d\n", ti32(i32(1267)));
    assert(ti32(i32(2147483646)) == 2147483646);
    assert(ti32(i32(-343646748)) == -343646748);

    printf("int64:      %s\n", fa_type_str(i64(12372)));
    printf("int64:      %lli\n", ti64(i64(9223372036854775807ll)));
    assert(ti64(i64(4872837827878787871ll)) == 4872837827878787871ll);
    assert(ti64(i64(-6888881236767676711ll)) == -6888881236767676711ll);

    printf("double:     %s\n", fa_type_str(f64(12372)));
    printf("double:     %f\n", tf64(f64(3.141592653589793)));
	// Does not work on Windows
    //assert(tf64(f64(3.141592653589793D)) == 3.141592653589793D);
    //assert(tf64(f64(-1.4142135623730951D)) == -1.4142135623730951D);
}


// --------------------------------------------------------------------------------

void test_generic_functions()
{
    test_section("Generic functions");
    // TODO leaks

    printf("2 * 3.2                      ==> %f\n",   tf64(fa_multiply(f64(2), f64(3.2))));
    printf("1 / 3                        ==> %f\n",   tf64(fa_divide(f64(1), f64(3))));
    printf("1 + 1.5                      ==> %f\n",   tf64(fa_add(f64(1), f64(1.5))));

    printf("32                  + 1      ==> %i\n",   ti8(fa_add(i8(32), i8(1))));
    printf("5123                + 1      ==> %i\n",   ti16(fa_add(i16(5123), i16(1))));
    printf("2147483646          + 1      ==> %i\n",   ti32(fa_add(i32(2147483646), i32(1))));
    printf("4872837827878787871 + 1      ==> %lli\n", ti64(fa_add(i64(4872837827878787871ll), i64(1))));
    printf("32                  - 1      ==> %i\n",   ti8(fa_subtract(i8(32), i8(1))));
    printf("5123                - 1      ==> %i\n",   ti16(fa_subtract(i16(5123), i16(1))));
    printf("2147483646          - 1      ==> %i\n",   ti32(fa_subtract(i32(2147483646), i32(1))));
    printf("4872837827878787871 - 1      ==> %lli\n", ti64(fa_subtract(i64(4872837827878787871ll), i64(1))));
    printf("3                   / 2      ==> %i\n",   ti8(fa_divide(i8(33), i8(2))));
    printf("3333                / 2      ==> %i\n",   ti16(fa_divide(i16(3333), i16(2))));
    printf("3333333333          / 2      ==> %i\n",   ti32(fa_divide(i32(3333333333l), i32(2))));
    printf("3333333333333333333 / 2      ==> %lli\n", ti64(fa_divide(i64(3333333333333333333ll), i64(2))));
    printf("3                   / 1      ==> %i\n",   ti8(fa_divide(i8(32), i8(1))));

    printf("true == false                ==> %s\n", (fa_equal(fb(true), fb(true))) ? "true" : false);
    printf("32   == 32                   ==> %s\n", (fa_equal(i8(32), i8(32))) ? "true" : false);
    printf("5123 == 5123                 ==> %s\n", (fa_equal(i16(5123), i16(5123))) ? "true" : false);


}



// --------------------------------------------------------------------------------

static inline void memdump(void *s, size_t n)
{
    for (size_t i = 0; i < n; ++i) {
        printf("%x ", *((unsigned char *)(s + i)));
    }

    printf("\n");
}

void test_string()
{
    test_section("Strings");
    {
        string_t s = fa_string_single('v');
        fa_dprint("str: %s\n", s);
    }

    {
        // char* cs = " 新隶体 "; // length 5
        char *cs = "höglund";

        string_t s = string(cs);
        printf("len: %i\n", fa_string_length(s));
        fa_print("str: %s\n", s);

        printf("charAt 0: %x\n", char_at(0, s));
        printf("charAt 1: %x\n", char_at(1, s));
        printf("charAt 2: %x\n", char_at(2, s));
        fa_destroy(s);
    }

    {
        string_t s = string("foo");
        string_t t = string("bar");
        string_t u = string_append(s, t);
        fa_dprint("str: %s\n", s);
        fa_dprint("str: %s\n", t);
        fa_dprint("str: %s\n", u);
    }

    {
        string_t s = string("foo");
        string_t t = string("bar");
        fa_print("str: %s\n", s);
        fa_print("str: %s\n", t);
        {
            string_t u = string_dappend(s, t);
            fa_dprint("str: %s\n", u);
        }
    }
    {
        string_t s = string("Foo, Bar, Baz");
        string_t t = fa_string_copy(s);
        fa_dprint("str: %s\n", s);
        fa_dprint("str: %s\n", t);
    }

    /*
        {
            string_t s = string("FooBarBaz");
            string_t t = fa_string_join_map(apply1, fa_string_single, s);
            fa_dprint("str: %s\n", s);
            fa_dprint("str: %s\n", t);
        }
    */

    {
        string_t s = string("A double quote: \", A backslash: \\");
        fa_dprint("str: %s\n", s);
    }
}


// --------------------------------------------------------------------------------

void test_show()
{
    test_section("Show");
    fa_print("\n", NULL);
    fa_dprint("%s\n", fb(0));
    fa_dprint("%s\n", i8(129));
    fa_dprint("%s\n", i16(129));
    fa_dprint("%s\n", i32(64000));
    fa_dprint("%s\n", f64(3.1415));
    fa_dprint("%s\n", empty());
    fa_dprint("%s\n", list(i8(1)));
    fa_dprint("%s\n", list(i8(1), i8(2), list(i8(1), i8(2), fb(true))));
    fa_dprint("%s\n", list(
                  pair(string("hans"), string("höglund")),
                  pair(string("lisa"), string("streich")),
                  pair(string("mats"), string("erlandsson"))));
}


// --------------------------------------------------------------------------------

void test_compare()
{
    test_section("Comparison");
    fa_dprint("\"abc\" <  \"abd\"               ==> %s\n", fb(fa_less_than(string("abc"), string("abd"))));
    fa_dprint("\"abc\" <= \"abd\"               ==> %s\n", fb(fa_less_than_equal(string("abc"), string("abd"))));
    fa_dprint("\"abc\" >  \"abd\"               ==> %s\n", fb(fa_greater_than(string("abc"), string("abd"))));
    fa_dprint("\"abc\" >= \"abd\"               ==> %s\n", fb(fa_less_than_equal(string("abc"), string("abd"))));
}


// --------------------------------------------------------------------------------

void test_rational()
{
    test_section("Rational numbers");
    fa_dprint("1/3 <  1/2                   ==> %s\n", fb(fa_less_than(ratio(1, 3), ratio(1, 2))));
    fa_dprint("1/3 >  1/2                   ==> %s\n", fb(fa_greater_than(ratio(1, 3), ratio(1, 2))));
    fa_dprint("1/3 == 2/6                   ==> %s\n", fb(fa_equal(ratio(1, 3), ratio(2, 6))));
    fa_dprint("1/3 == 254/762               ==> %s\n", fb(fa_equal(ratio(1, 3), ratio(254, 762))));
    fa_dprint("1/3 <= 7/8                   ==> %s\n", fb(fa_equal(ratio(1, 3), ratio(254, 762))));
}


// --------------------------------------------------------------------------------

void test_buffer()
{
    test_section("Buffers");

    {
        fa_buffer_t b = fa_buffer_create(16);

        fa_print("b                            ==> %s\n", b);

        for (int i = 0; i < 16; ++i) {
            fa_buffer_set(b, i, i);
        }

        fa_print("b                            ==> %s\n", b);

        for (int i = 0; i < 16; ++i) {
            fa_buffer_set(b, i, 0xff);
        }

        fa_print("b                            ==> %s\n", b);
        fa_print("size(b)                      ==> %s\n", i32(fa_buffer_size(b)));
        fa_destroy(b);
    }

    {
        fa_buffer_t b = fa_buffer_create(1024);

        fa_print("b                            ==> %s\n", b);

        for (int i = 0; i < 1024; ++i) {
            fa_buffer_set(b, i, i);
        }

        fa_print("b                            ==> %s\n", b);

        for (int i = 0; i < 1024; ++i) {
            fa_buffer_set(b, i, 0xff);
        }

        fa_print("b                            ==> %s\n", b);
        fa_print("size(b)                      ==> %s\n", i32(fa_buffer_size(b)));
        fa_destroy(b);
    }
}


// --------------------------------------------------------------------------------

void test_time()
{
    test_section("Time");

    fa_time_t t = fa_time_create(1, 0, 0, ratio(25, 8));
    fa_time_t u = fa_time_create(0, 1, 1, ratio(58, 1));

    fa_print("t                            ==> %s\n", t);
    fa_print("u                            ==> %s\n", u);
    fa_print("t + u                        ==> %s\n", fa_add(t, u));

    fa_print("fa_time_to_iso(t)       ==> %s\n", fa_time_to_iso(t));
    fa_print("fa_time_to_iso(u)       ==> %s\n", fa_time_to_iso(u));


    fa_print("200 ms       ==> %s\n", fa_milliseconds(200));
    fa_print("200 ms       ==> %s\n", fa_time_create(0, 0, 0, ratio(1, 5)));

    fa_destroy(t);
    fa_destroy(u);
}


// --------------------------------------------------------------------------------

// void test_system_time()
// {
//     test_section("System time");
//
//     clock_t system_clock = fa_time_get_system_prec_clock();
//
//     for (int i = 0; i < 10; ++i) {
//         // fa_print("system()                     ==> %s\n", fa_time_system());
//         // fa_print("cpu()                        ==> %s\n", fa_time_cpu());
//         // fa_print("system()                     ==> %s\n", fa_time_from_system(fa_time_system()));
//         // fa_print("cpu()                        ==> %s\n", fa_time_from_cpu(fa_time_cpu()));
//
//         fa_print("time(systemClock)            ==> %s\n", fa_time_time(system_clock));
//         fa_print("ticks(systemClock)           ==> %s\n", i64(fa_time_ticks(system_clock)));
//
//         fa_thread_sleep(50);
//     }
// }

// --------------------------------------------------------------------------------

void test_midi_message()
{
    test_section("Midi");

    {
        fa_midi_message_t m = fa_midi_message_create_simple(0xa, 60, 127);
        fa_dprint("m                            ==> %s\n", m);
    }

    {
        fa_buffer_t b = fa_buffer_create(32);

        for (int i = 0; i < 32; ++i) {
            fa_buffer_set(b, i, i);
        }

        fa_midi_message_t m = fa_midi_message_create_sysex(b);
        fa_dprint("m                            ==> %s\n", m);
    }
}


// --------------------------------------------------------------------------------

// void test_type()
// {
//     test_section("Types");
//
//     fa_dprint("type(i8)                  ==> %s\n", type(i8));
//     // fa_dprint("size_of(1024,type(i8))    ==> %s\n", i32(fa_type_size_of(1024, type(i8))));
//     // fa_dprint("align_of(1024,type(i8))   ==> %s\n", i32(fa_type_align_of(type(i8))));
//     printf("\n");
//
//     // fa_dprint("type(f64)                 ==> %s\n", type(f64));
//     // fa_dprint("size_of(1024,type(f64))   ==> %s\n", i32(fa_type_size_of(1024, type(f64))));
//     // fa_dprint("align_of(1024,type(f64))  ==> %s\n", i32(fa_type_align_of(type(f64))));
//     // printf("\n");
//     //
//     // type_t t = type_pair(type(i8), type(f64));
//     // fa_dprint("t                            ==> %s\n", t);
//     // fa_dprint("size_of(1024,t)              ==> %s\n", i32(fa_type_size_of(1024, t)));
//     // fa_dprint("align_of(1024,t)             ==> %s\n", i32(fa_type_align_of(t)));
//     // printf("\n");
//     //
//     // type_t u = type_pair(type_vector(type(i8), 10), type(f64));
//     // fa_dprint("u                            ==> %s\n", u);
//     // fa_dprint("size_of(1024,u)              ==> %s\n", i32(fa_type_size_of(1024, u)));
//     // fa_dprint("align_of(1024,u)             ==> %s\n", i32(fa_type_align_of(u)));
//     // printf("\n");
//     //
//     // type_t u2 = type_pair(type_frame(type(i8)), type(f64));
//     // fa_dprint("u2                           ==> %s\n", u2);
//     // fa_dprint("size_of(1024,u2)             ==> %s\n", i32(fa_type_size_of(1024, u2)));
//     // fa_dprint("align_of(1024,u2)            ==> %s\n", i32(fa_type_align_of(u2)));
//     // printf("\n");
//
//     type_t v = type_pair(type(i8), type_pair(type(i8), type_pair(type(i8),
//                                              type_pair(type(i8), type_pair(type(i8), type_pair(type(i8),
//                                                        type_pair(type(i8), type_pair(type(i8), type_pair(type(i8),
//                                                                type(i8))))))))));
//
//     fa_print("v                            ==> %s\n", v);
//     // fa_dprint("size_of(1024,v)              ==> %s\n", i32(fa_type_size_of(1024, v)));
//     // fa_dprint("align_of(1024,v)             ==> %s\n", i32(fa_type_align_of(v)));
//     fa_destroy(v);
//     printf("\n");
// }



// --------------------------------------------------------------------------------

#pragma mark -

ptr_t add10(ptr_t x, ptr_t _)
{
    return (ptr_t)((intptr_t) x + 10);
}

void test_atomic()
{
    test_section("Atomic");

    // treat as integer
    {
        fa_atomic_t a = fa_atomic_create();
        fa_print("a                            ==> %s\n", a);

        fa_atomic_set(a, (ptr_t) 0x5);
        fa_print("a                            ==> %s\n", a);

        fa_atomic_modify(a, add10, 0);
        fa_print("a                            ==> %s\n", a);

        // fa_atomic_add(a, (ptr_t) - 0xf);
        // fa_print("a                            ==> %s\n", a);

        fa_atomic_exchange(a, (ptr_t) 1, (ptr_t) 0xfe);
        fa_print("a                            ==> %s\n", a); // fails, still 0

        fa_atomic_exchange(a, (ptr_t) 0, (ptr_t) 0xff);
        fa_print("a                            ==> %s\n", a); // now ff
    }
}


// --------------------------------------------------------------------------------

struct reader_args {
    fa_atomic_queue_t queue;
    atomic_t active;
};

fa_ptr_t queue_reader(fa_ptr_t x)
{
    struct reader_args *args = x;
    fa_atomic_queue_t q = args->queue;
    atomic_t               a = args->active;
    ptr_t                  v;

    while (true) {
        if (!tb(fa_atomic_get(a))) {
            return v;
        }

        if ((v = fa_atomic_queue_read(q))) {
            printf("         |- %5d    \n", ti32(v));
        }

        fa_thread_sleep(rand() % 100);
    }
}

void test_atomic_queue(int iter, long sleepTime)
{
    test_section("Atomic queues");
    {
        fa_atomic_queue_t q = fa_atomic_queue_create();

        struct reader_args args = { q, atomic() };
        fa_atomic_set(args.active, fb(true));

        thread_t t = fa_thread_create(queue_reader, &args);

        fa_print("q                            ==> %s\n", q);

        for (int i = 0; i < iter; ++i) {
            fa_thread_sleep(rand() % 100);
            fa_atomic_queue_write(q, i32(i));
            printf("  %5d -|  \n", i);
        }

        fa_thread_sleep(sleepTime);
        fa_atomic_set(args.active, fb(false));
        fa_thread_join(t); // TODO how to kill?
        fa_destroy(q);
    }
}


// --------------------------------------------------------------------------------

struct stack_reader_args {
    fa_atomic_stack_t stack;
    atomic_t active;
};

fa_ptr_t stack_reader(fa_ptr_t x)
{
    struct stack_reader_args *args = x;
    fa_atomic_stack_t q = args->stack;
    atomic_t               a = args->active;
    ptr_t                  v;

    while (true) {
        if (!tb(fa_atomic_get(a))) {
            return v;
        }

        if ((v = fa_atomic_stack_read(q))) {
            printf("         |- %5d    \n", ti32(v));
        }

        srand(time(NULL));
        fa_thread_sleep(rand() % 100);
    }
}

void test_atomic_stack(int iter, long sleepTime)
{
    test_section("Atomic stacks");
    {
        fa_atomic_stack_t q = fa_atomic_stack_create();

        struct stack_reader_args args = { q, atomic() };
        fa_atomic_set(args.active, fb(true));

        thread_t t = fa_thread_create(stack_reader, &args);

        fa_print("q                            ==> %s\n", q);

        for (int i = 0; i < iter; ++i) {
            if (i % 10) {
                fa_thread_sleep(rand() % 100);
            }

            fa_atomic_stack_write(q, i32(i));
            printf("  %5d -|  \n", i);
        }

        fa_thread_sleep(sleepTime);
        fa_atomic_set(args.active, fb(false));
        fa_thread_join(t);
        fa_destroy(q);
    }
}


// --------------------------------------------------------------------------------

void test_atomic_ring_buffer(int inter, long sleepTime)
{
    test_section("Ring buffer");
}


// --------------------------------------------------------------------------------

ptr_t printer(ptr_t data)
{
    int n = 0;

    while (n < 100) {
        printf("%d\n", n);
        n = n + ((intptr_t) data);
        fa_thread_sleep(100);
    }

    return 0;
}

void test_thread()
{
    test_section("Threads");

    fa_thread_t t, t2;
    t  = fa_thread_create(printer, (ptr_t) 10);
    t2 = fa_thread_create(printer, (ptr_t) 11);

    fa_thread_sleep(1000);
    fa_thread_join(t);
    fa_thread_join(t2);
}


// --------------------------------------------------------------------------------

typedef struct {
    fa_thread_mutex_t mut;
    int val;
} lock_index;

ptr_t locker(ptr_t x)
{
    lock_index *i = (lock_index *) x;

    fa_thread_lock(i->mut);
    printf("Acquired lock in thread %d\n", i->val);
    fa_thread_sleep(200);
    fa_thread_unlock(i->mut);
    printf("Released lock in thread %d\n", i->val);

    return 0;
}

void test_mutex()
{
    test_section("Mutexes");

    fa_thread_mutex_t m = fa_thread_create_mutex();

    for (int j = 0; j < 10; ++j) {
        lock_index i = { m, j };
        fa_thread_t t = fa_thread_create(locker, (ptr_t) &i);
        fa_thread_sleep(100);
        fa_thread_detach(t);
    }

    fa_thread_sleep(1200);
}


// --------------------------------------------------------------------------------

#pragma mark -

void test_for_each()
{
    test_section("For each loops");

    fa_let(x, 33) {
        fa_let(y, 1)
        fa_let(z, x + y)
        fa_print("%s\n", i32(z));
    }

    printf("\n");
    fa_with(list, list(i32(1), i32(2), i32(3), i32(4)), fa_destroy(list)) {
        fa_for_each(x, list) {
            fa_print(">    %s\n", x);
        }
    }

    printf("\n");
    fa_with(set, set(i32(1), i32(1), i32(2), i32(1)), fa_destroy(set)) {
        fa_for_each(x, fa_set_to_list(set)) {
            fa_print(">    %s\n", x);
        }
    }

    printf("\n");

    fa_with(map, map(
                string("foo"), i16(1),
                string("bar"), list(i16(1), i16(2), i16(3))),
            fa_destroy(map)) {
        fa_for_each(x, fa_map_to_list(map)) {
            fa_print(">    %s\n", x);
        }
    }
}



// --------------------------------------------------------------------------------

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

// x = [x,x]
ptr_t dup_list(ptr_t ct, ptr_t x)
{
    return list(x, x);
}

void test_list()
{
    test_section("List");

    {
        printf("\n");

        list_t as = empty();
        fa_print("empty()                      ==> %s\n", as);
        fa_destroy(as);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3));
        list_t bs = fa_list_cons(i16(0), as);

        fa_print("as                           ==> %s\n", as);
        fa_print("cons(0,as)                   ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3));
        list_t bs = fa_list_append(as, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("append(as,as)                ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3));
        list_t bs = fa_list_copy(as);
        fa_print("as                           ==> %s\n", as);
        fa_print("copy(as)                     ==> %s\n", bs);
        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3));
        list_t bs = fa_list_init(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("init(as)                     ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3));
        ptr_t v = fa_list_last(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("last(as)                     ==> %s\n", v);

        fa_destroy(as);
        fa_destroy(v);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3));

        fa_print("as                           ==> %s\n", as);
        fa_print("length(as)                   ==> %s\n", i16(fa_list_length(as)));

        fa_destroy(as);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_reverse(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("reverse(as)                  ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    {
        printf("\n");

        // list_t as = list(i16(1), i16(-2), i16(0), i16(4), i16(123));
        list_t as = fa_list_enumerate(0, 10);
        as = fa_list_reverse(as);
        as = fa_list_dmap(apply1, i32, as);

        list_t bs = fa_list_sort(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("sort(as)                     ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_take(3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("take(3,as)                   ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_drop(3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("drop(3,as)                   ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        ptr_t v = fa_list_index(1, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("index(1,as)                  ==> %s\n", v);

        fa_destroy(as);
    }

    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_range(1, 3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("range(1,3,as)                ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_remove_range(1, 3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("removeRange(1,3,as)          ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t xs = list(i16(0), i16(0));
        list_t bs = fa_list_insert_range(2, xs, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("insertRange(2,list(0,0),as)  ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(xs);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_insert(2, i16(0), as);

        fa_print("as                           ==> %s\n", as);
        fa_print("insert(2,0,as)               ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_insert(0, i16(0), as);

        fa_print("as                           ==> %s\n", as);
        fa_print("insert(0,1,as)               ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        list_t as = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t bs = fa_list_remove(2, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("remove(2,as)                 ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    // has
    // find
    // findIndex

    {
        printf("\n");

        list_t as = list(i16(1), i16(3), i16(5));

        fa_print("as                           ==> %s\n", as);
        fa_print("indexOf(0,as)                ==> %s\n", i16(fa_list_index_of(i16(0), as)));
        fa_print("indexOf(1,as)                ==> %s\n", i16(fa_list_index_of(i16(1), as)));
        fa_print("indexOf(2,as)                ==> %s\n", i16(fa_list_index_of(i16(2), as)));
        fa_print("indexOf(3,as)                ==> %s\n", i16(fa_list_index_of(i16(3), as)));
        fa_print("indexOf(4,as)                ==> %s\n", i16(fa_list_index_of(i16(4), as)));
        fa_print("indexOf(5,as)                ==> %s\n", i16(fa_list_index_of(i16(5), as)));

        fa_destroy(as);
    }
    {
        printf("\n");

        list_t xs = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t ys = fa_list_filter(is_odd16, 0, xs);

        fa_print("xs                           ==> %s\n", xs);
        fa_print("filter(is_odd,ys)            ==> %s\n", ys);

        fa_destroy(xs);
        fa_destroy(ys);
    }

    {
        printf("\n");

        list_t xs = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t ys = fa_list_map(times10, 0, xs);

        fa_print("xs                           ==> %s\n", xs);
        fa_print("map(times10,ys)              ==> %s\n", ys);

        fa_destroy(xs);
        fa_destroy(ys);
    }

    {
        printf("\n");

        list_t xs = list(i16(1), i16(2), i16(3), i16(4), i16(5));
        list_t ys = list(i16(66), i16(77));
        list_t zss = list(xs, ys, xs);

        fa_print("[xs,ys]                      ==> %s\n", zss);
        fa_print("join([xs,ys])                ==> %s\n", fa_list_join(zss));

        fa_destroy(xs);
        fa_destroy(ys);
    }


    // {
    //     printf("\n");
    // 
    //     list_t xs = list(i16(1), i16(2), i16(3), i16(4), i16(5));
    //     list_t ys = fa_list_join_map(dup_list, 0, xs);
    // 
    //     fa_print("xs                           ==> %s\n", xs);
    //     fa_print("joinMap(\\x -> [x,x])         ==> %s\n", ys);
    // 
    //     fa_destroy(xs);
    //     fa_destroy(ys);
    // }


    {
        printf("\n");

        list_t xs = fa_list_enumerate(0, 50000);

        xs = fa_list_dreverse(xs);
        // fa_print("reverse(xs)                  ==> %s\n", xs);

        xs = fa_list_dmap(apply1, i16, xs);
        // fa_print("xs                           ==> %s\n", xs);

        xs = fa_list_dfilter(is_odd16, 0, xs);
        // fa_print("filter(is_odd,xs)            ==> %s\n", xs);

        xs = fa_list_dmap(times10, 0, xs);
        // fa_print("map(times10, xs)             ==> %s\n", xs);

        fa_destroy(xs);
    }
    {
        printf("\n");

        list_t xs = fa_list_enumerate(0, 12);
        xs = fa_list_dmap(apply1, i8, xs);

        fa_print("xs                           ==> %s\n", xs);
        ptr_t sum = fa_list_dfold_left(apply2, fa_add, i8(0), xs);
        fa_print("sum(xs)                      ==> %s\n", sum);
    }
}


// --------------------------------------------------------------------------------

void test_set()
{
    test_section("Set");
    {
        printf("\n");

        set_t a = set(i16(1), i16(3), i16(2));

        a = fa_set_dadd(i16(1), a);
        a = fa_set_dadd(i16(5), a);
        a = fa_set_dadd(i16(3), a);
        a = fa_set_dremove(i16(3), a);

        fa_print("a                            ==> %s\n", a);
        fa_print("size(a)                      ==> %s\n", i16(fa_set_size(a)));
        fa_destroy(a);
    }

    {
        printf("\n");

        set_t a = set(i16(1), i16(2), i16(3));
        set_t b = set(i16(3), i16(4));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a + b                        ==> %s\n", fa_set_sum(a, b));

        fa_destroy(a);
        fa_destroy(b);
    }

    {
        printf("\n");

        set_t a = set(i16(1), i16(2), i16(3));
        set_t b = set(i16(3), i16(4));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a - b                        ==> %s\n", fa_set_difference(a, b));

        fa_destroy(a);
        fa_destroy(b);
    }

    {
        printf("\n");

        set_t a = set(i16(1), i16(2), i16(3));
        set_t b = set(i16(3), i16(4));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a x b                        ==> %s\n", fa_set_product(a, b));

        fa_destroy(a);
        fa_destroy(b);
    }

    {
        printf("\n");

        set_t a = set(string("foo"), string("bar"));
        set_t b = set(string("hi"), string("ho"));
        set_t c = set(i16(0), i16(1));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a x b                        ==> %s\n", fa_set_product(a, b));

        set_t ab = fa_set_product(a, b);
        fa_dprint("a x b x c                    ==> %s\n", fa_set_product(ab, c));
        fa_destroy(ab);

        fa_destroy(a);
        fa_destroy(b);
        fa_destroy(c);
    }
}


// --------------------------------------------------------------------------------

void test_map()
{
    test_section("Map");
    {
        printf("\n");

        map_t a = fa_map_empty();

        a = fa_map_dadd(string("name"), string("Hans"), a);
        a = fa_map_dset(string("name"), string("Sven"), a);
        a = fa_map_dset(string("age"), i16(22), a);
        a = fa_map_dset(string("age"), i16(22), a);

        // a = fa_map_dremove(string("age"), a);
        // a = fa_map_dadd(string("age"), i16(25), a);
        a = fa_map_dset(string("skills"), list(string("programming"), string("composition")), a);

        // a = fa_map_dadd(string("happy"), fb(true), a);
        // a = fa_map_dadd(string("pair"), pair(fb(true), f64(3.1415)), a);
        // a = fa_map_dadd(string("ratio"), ratio(1, 3), a);
        // a = fa_map_dadd(string("ratio2"), fa_multiply(ratio(4, 4444), ratio(1, 2)), a);

        fa_print("a                            ==> %s\n", a);
        fa_print("size(a)                      ==> %s\n", i16(fa_map_size(a)));

        fa_print("a.name                       ==> %s\n", fa_map_get(string("name"), a));
        fa_print("a.age                        ==> %s\n", fa_map_get(string("age"), a));
        fa_print("a.skills                     ==> %s\n", fa_map_get(string("skills"), a));
        fa_print("a.happy                      ==> %s\n", fa_map_get(string("happy"), a));
        fa_print("a.pair                       ==> %s\n", fa_map_get(string("pair"), a));

        fa_destroy(a);
    }
}


// --------------------------------------------------------------------------------

// void test_graph(string_t path)
// {
//     test_section("Graph");
//     {
//         graph_t a = fa_graph_empty();
//
//
//         a = fa_graph_insert(string("foo"), a);
//         a = fa_graph_connect(string("foo"), string("foo"), string("(1)"), a);
//
//         // a = fa_graph_insert(pair(string("a"), string("b")), a);
//         // a = fa_graph_connect(
//         //     pair(string("a"), string("b")),
//         //     pair(string("a"), string("b")),
//         //     string("(1)"), a);
//
//         fa_print("a                            ==> %s\n", a);
//         fa_system_directory_write_file(path, fa_graph_to_dot(
//                                                 string("#include \"doc/graphs/header.dot\""),
//                                                 string(""),
//                                                 a));
//     }
// }


// --------------------------------------------------------------------------------

void test_priority_queue(int iter)
{
    test_section("Priority queue");

    priority_queue_t q = fa_priority_queue_empty();
    srand(time(NULL));

    for (int i = 0; i < iter; ++i) {
        fa_priority_queue_insert(fa_add(hours(rand() % 24), seconds(rand() % 3600)), q);
    }

    while (fa_priority_queue_peek(q)) {
        fa_dprint("     -> %s \n", fa_priority_queue_pop(q));
    }

}


// --------------------------------------------------------------------------------

void test_json(string_t path)
{
    extern void fa_puts(fa_string_t string);

    test_section("JSON conversion");

    string_t json = fa_system_directory_read_file(path);
    // printf("%s\n", unstring(json));

    ptr_t data = fa_string_from_json(json);
    fa_print("data                         ==> %s\n", data);

    string_t json2 = fa_string_to_json(data);
    fa_puts(json2);

    fa_puts(fa_string_to_json(
                pair(i32(1), i32(2))));

    fa_puts(fa_string_to_json(
                list(pair(i32(1), i32(2)), pair(i32(3), i32(4)))));

    fa_puts(fa_string_to_json(
                set(pair(i32(1), i32(2)), pair(i32(1), i32(2)))));

    fa_puts(fa_string_to_json(
                map(
                    string("foo"), i32(1),
                    string("bar"), list(i32(1), i32(2), i32(3)))));

}



// --------------------------------------------------------------------------------

#pragma mark -

// void test_dispatcher()
// {
//     test_section("Dispatcher");
//
//     dispatcher_t disp = lockfree_dispatcher();
//
//     ptr_t val = map(
//                     string("lyrics"), list(string("Help"), string("me"), string("if"), string("you"), string("can")),
//                     string("pitches"), list(ratio(60, 1), ratio(62, 1))
//                 );
//
//     fa_message_send((receiver_t) disp, i16(1), val);
//     fa_message_send((receiver_t) disp, i16(2), string("World!"));
//     fa_message_send((receiver_t) disp, i16(2), string("World!"));
//     fa_message_send((receiver_t) disp, i16(2), string("World!"));
//     fa_message_send((receiver_t) disp, i16(2), string("World!"));
//
//     list_t msgs = fa_list_empty();
//
//     while (true) {
//         fa_message_sync((sender_t) disp);
//         msgs = fa_message_receive((sender_t) disp, i16(1));
//
//         if (fa_list_is_empty(msgs)) {
//             break;
//         }
//
//         fa_print("             | 1: %s\n", msgs);
//     }
//
//     fa_destroy(disp);
// }
//

// --------------------------------------------------------------------------------

// void test_system_event()
// {
//     test_section("System events");
//
//     fa_message_sender_t s =
//         fa_system_event_receive(
//             list(
//                 // i16(mouse_move_event)
//                 // i16(mouse_down_event)
//                 i16(key_down_event),
//                 i16(key_up_event)
//
//             ));
//     fa_message_receiver_t r =
//         fa_system_event_send_std();
//
//     for (int i = 0; i < 100000; ++i) {
//         fa_message_sync(s);
//         fa_for_each(x, fa_message_receive(s, i16(0))) {
//             // fa_print("    Received: %s\n", x);
//             fa_message_send(r, i16(0), x);
//         }
//         fa_thread_sleep(5);
//     }
// }


// --------------------------------------------------------------------------------

// void test_event()
// {
//     {
//         // fa_time_t t = seconds(0);
//
//         // event_t ha = now(string("höglund"));
//         // event_t ho = now(string("holmgren"));
//
//         // event_t a = merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha, never())))))))))));
//
//         // event_t b = merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho, never())))))))))));
//
//         // event_t s1 = fa_event_later(seconds(1), NULL);
//         // event_t s3 = fa_event_later(seconds(3), NULL);
//
//         event_t mm = fa_system_event_mouse_move();
//         // event_t md = fa_system_event_mouse_down();
//         // event_t mu = fa_system_event_mouse_up();
//         // event_t kd = fa_system_event_key_down();
//         // event_t ku = fa_system_event_key_up();
//         // event_t mouseX = fa_event_map(apply1, fa_pair_first, mm);
//         // event_t mouseY = fa_event_map(apply1, fa_pair_second, mm);
//
//         // event_t y2 = merge_event(switch_event(kd, merge_event(a, mm), merge_event(b, md)), later(seconds(5), list(string("flux"))));
//         // event_t y2 = switch_event(ku, switch_event(kd,never(),mm), merge_event(delay_event(seconds(3),b),md));
//         // event_t y2 = switch_event(kd,mm,merge_event(md,mu));
//         // event_t y2 = fa_event_filter(fa_less_than, f64(500), mouseX);
//         event_t y2 = mm;
//         // fa_print("The event: %s\n", mouseX);
//         event_t z  = fa_system_event_write_std(y2);
//
//         {
//             clock_t     clk = fa_time_get_system_prec_clock();
//             scheduler_t sched = fa_scheduler_create(clk);
//             fa_scheduler_schedule(sched, z);
//             fa_scheduler_loop(sched);
//         }
//     }
// }


// --------------------------------------------------------------------------------

void test_scheduler()
{
    test_section("Scheduler");
    // TODO
}


// --------------------------------------------------------------------------------

ptr_t add1234(ptr_t c, ptr_t x)
{
    return i8(ti8(x) + 1234);
}

// void test_processor_graphs(string_t path)
// {
//     test_section("Processors");
//
//     inform(string_append(string("Writing "), path));
//
//     processor_t p, q, chain, rchain;
//     p = unary(type(i8), type(i8), add1234, NULL);
//     chain = seq(p, seq(p, seq(p, seq(p, seq(p, p)))));
//     rchain = seq(seq(seq(seq(p, p), p), p), p);
//     q =
//         seq(split(type(i8)),
//
//             par(
//                 seq(seq(seq(p, p), p), p),
//
//                 seq(
//                     split(type(i8)),
//                     par(seq(split(type(i8)), par(chain, seq(split(type(i8)), par(chain, chain)))),
//                         seq(split(type(i8)), par(p, seq(p, p))))
//                 )
//
//             )
//
//
//            )
//
//         ;
//
//     fa_processor_write_graph(q, path);
// }


// --------------------------------------------------------------------------------

ptr_t cont(ptr_t x)
{
    printf("Continuing...\n");
    return x;
}

double f1(void *ct, int i, double t, double x)
{
    double pi  = 3.141592653589793;
    double tau = 2 * pi;
    double t0  = x;
    // double t2  = t + x;
    // t2 = t2;

#define step(p) ((float)((int)fmod(t,p)%p))/p

    switch (i) {
    case 3:
        return step(5);

    case 2:
        return -0.5 * cos(tau * t0 * 0.5 + pi);

    case 1:
        return  0.5 * cos(tau * t0 * 0.5 + pi);

    case 0:
        return  0.5 * cos(tau * t0 * 0.5 + pi) * sin(tau * t0 * 3);

    default:
        return 0;
    }
}

void test_plot()
{
    test_section("Plot");
    fa_plot_continous(f1, NULL, NULL, NULL);
}


// --------------------------------------------------------------------------------

void test_plot_buffer()
{
    buffer_t buf = fa_buffer_create(44100 * sizeof(double));

    for (int i = 0; i < 44100; ++i) {
        double r = (double) rand() / RAND_MAX;
        double x = (double) i / 44100;
        fa_buffer_set_double(buf, i, (r * 2 - 1) * sin(x * 10));
    }

    fa_plot_buffer_double(buf, NULL, NULL);
}


// --------------------------------------------------------------------------------

void test_plot_file(string_t path)
{
    test_section("Plot file");

    pair_t res = fa_buffer_read_audio(path);

    if (fa_error_check(res)) {
        fa_error_log(NULL, (error_t) res);
        return;
    }

    fa_print("%s\n", res);

    buffer_t buf = fa_pair_second(res);
    fa_plot_buffer_double(buf, NULL, NULL);
    fa_destroy(buf);
    fa_destroy(res);
}


// --------------------------------------------------------------------------------

void test_error()
{

}


// --------------------------------------------------------------------------------

void test_log()
{
    test_section("Logging");
    fa_fa_log_info(string("---------------"));
    fa_fa_log_info(string("Log test: Do not take these seriously"));

    for (int i = 0; i < 3; ++i) {

        fa_fa_log_info(string("We have a problem"));
        // fa_fa_log_warning(string("We have a problem"));
        fa_fa_log_error(string("We have a problem"));

        fa_fa_log(NULL,
                  fa_error_create_simple(
                      error,
                      string("We have a problem"),
                      string("Doremir.FooBar")));
        fa_thread_sleep(50);
    }

    fa_fa_log_info(string("---------------"));
}


// --------------------------------------------------------------------------------

void test_system_directory()
{
    test_section("Directory");

    fa_print("home()                       ==> %s\n", fa_system_directory_home());
    fa_print("current()                    ==> %s\n", fa_system_directory_current());
}



// --------------------------------------------------------------------------------

void test_regex()
{
    test_section("Regular expressions");

    char exp[] = ".* Hans H.*";
    char str[] = "A Hans Höglund";

    fa_print("exp                          ==> %s\n", string(exp));
    fa_print("str                          ==> %s\n", string(str));
    fa_print("matches(exp,str)             ==> %s\n", fb(fa_string_matches(string(exp), string(str))));
}

// --------------------------------------------------------------------------------

// void test_file_stream(string_t in_path, string_t out_path)
// {
//     test_section("File streams");
//
//     file_device_t    input, output;
//     file_result_t    result;
//     // processor_t proc;
//
//     // Processor to use
//     // proc    = fa_processor_identity(type_pair(type_frame(type(f32)), type_frame(type(f32))));
//     proc = NULL;
//
//     // Open streams
//     input   = fa_device_file_open(in_path);
//     output  = fa_device_file_open(out_path);
//
//     // Handle possible errors
//     if (fa_check(input)) {
//         log_error((error_t) input);
//         warn(string("Aborting test due to error"));
//         goto cleanup;
//     }
//
//     if (fa_check(output)) {
//         log_error((error_t) output);
//         warn(string("Aborting test due to error"));
//         goto cleanup;
//     }
//
//     result = fa_device_file_run(input, proc, output);
//
//     // Handle possible error
//     if (fa_check(result)) {
//         log_error((error_t) result);
//         warn(string("Aborting test due to error"));
//         goto cleanup;
//     }
//
// cleanup:
//     fa_device_file_close(input);
//     fa_device_file_close(output);
// }
//
//
// // --------------------------------------------------------------------------------
//
// void test_buffer_stream()
// {
//     test_section("Buffer streams");
// }
//

// --------------------------------------------------------------------------------

void print_audio_devices(audio_session_t session)
{
    fa_print("\n", NULL);
    fa_print("    Listing audio devices: \n", NULL);
    fa_for_each(x, fa_audio_all(session)) {
        fa_print("        Device: %s\n", x);
        fa_print("            Input:  %s\n", i32(fa_audio_input_channels(x)));
        fa_print("            Output: %s\n", i32(fa_audio_output_channels(x)));
    }
    fa_print("    Default input is : %s\n", fa_audio_default_input(session));
    fa_print("    Default output is : %s\n", fa_audio_default_output(session));
    fa_print("\n", NULL);
}


// --------------------------------------------------------------------------------

ptr_t status_changed(ptr_t ct)
{
    printf("Status changed: %s!\n", unstring(ct));
    return 0;
}

void test_audio_stream()
{
    test_section("Audio streams");

    audio_session_t session;
    audio_device_t  input, output;
    audio_stream_t  stream;
    // processor_t     proc1, proc2;

    // Processor to use
    // proc1    = id(type_pair(type_frame(type(f32)), type_frame(type(f32))));
    // proc2    = seq(proc1, proc1);

    // Begin session
    session = fa_audio_begin_session();

    // Handle possible error
    if (fa_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // Session obtained, we can now access devices
    print_audio_devices(session);

    input = fa_audio_default_input(session);
    output = fa_audio_default_output(session);

    // Start stream
    stream = fa_audio_open_stream(input, NULL, NULL, output);

    // Handle possible error
    if (fa_check(stream)) {
        log_error((error_t) stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    fa_audio_add_status_callback(status_changed, string("foobar"), session);
    fa_thread_sleep(3000);

cleanup:
    fa_audio_close_stream(stream);
    fa_audio_end_session(session);
}



// --------------------------------------------------------------------------------

void print_midi_devices(midi_session_t session)
{
    fa_print("\n", NULL);
    fa_print("    Listing midi devices: \n", NULL);
    fa_for_each(x, fa_midi_all(session)) {
        fa_print("        Device: %s\n", x);
        fa_print("            Input:  %s\n", fb(fa_midi_has_input(x)));
        fa_print("            Output: %s\n", fb(fa_midi_has_output(x)));
    }
    fa_print("    Default input is : %s\n", fa_midi_default_input(session));
    fa_print("    Default output is : %s\n", fa_midi_default_output(session));
    fa_print("\n", NULL);
}

ptr_t to_note_on(ptr_t occ)
{
    // fa_print("%s\n", occ);
    int16_t kc = ti16(fa_list_head(occ));
    return midi_message(0x90, 48 + kc, 120);
}

ptr_t to_note_off(ptr_t occ)
{
    // fa_print("%s\n", occ);
    int16_t kc = ti16(fa_list_head(occ));
    return midi_message(0x80, 48 + kc, 120);
}

ptr_t to_control(ptr_t occ)
{
    // fa_print("%s\n", occ);
    double x = tf64(fa_pair_first(occ));
    return midi_message(0xb0, 7, x / 1900 * 127);
}
ptr_t to_control2(ptr_t occ)
{
    // fa_print("%s\n", occ);
    double y = tf64(fa_pair_second(occ));
    return midi_message(0xb0, 1, y / 1200 * 127);
}


void test_midi_stream()
{
    test_section("Midi streams");

    midi_session_t session;
    midi_device_t  input, output;
    midi_stream_t  in_stream, out_stream;

    // Begin session
    session = fa_midi_begin_session();

    // Handle possible error
    if (fa_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // Session obtained, we can now access devices
    print_midi_devices(session);

    input = fa_list_index(2, fa_midi_all(session));
    // output = fa_midi_default_output(session);
    output = fa_list_index(6, fa_midi_all(session));

    // Start streams
    in_stream  = fa_midi_open_stream(input);
    out_stream = fa_midi_open_stream(output);

    // Handle possible errors
    if (fa_check(in_stream)) {
        log_error((error_t) in_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    if (fa_check(out_stream)) {
        log_error((error_t) out_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // TODO
    // fa_midi_add_status_callback(status_changed, string("foobar"), session);

    // event_t notes  =
    //     merge_event(later(divisions(1,10), midi(0x90, 48, 10)),
    //     merge_event(later(divisions(2,10), midi(0x90, 50, 20)),
    //     merge_event(later(divisions(3,10), midi(0x90, 52, 30)),
    //     merge_event(later(divisions(4,10), midi(0x90, 53, 40)),
    //     merge_event(later(divisions(5,10), midi(0x90, 55, 50)),
    //     merge_event(later(divisions(6,10), midi(0x90, 57, 60)),
    //     merge_event(later(divisions(7,10), midi(0x90, 59, 70)),
    //     merge_event(later(divisions(8,10), midi(0x90, 60, 80)),
    //     never()))))))));

    // event_t notes =
    //     merge_event(fa_event_map(apply1, to_note_on,  fa_system_event_key_down()),
    //     merge_event(fa_event_map(apply1, to_note_off, fa_system_event_key_up()),
    //     merge_event(fa_event_map(apply1, to_control,  fa_system_event_mouse_move()),
    //                 fa_event_map(apply1, to_control2, fa_system_event_mouse_move()))));

    // event_t notes2 = fa_event_before(later(seconds(3),0), notes);

    // event_t notes   = fa_event_receive((sender_t) in_stream, i32(0));
    // event_t sender  = fa_event_send((receiver_t) out_stream, i32(0), notes);
    // event_t sender2 = fa_system_event_write_std(notes);

    // scheduler_t sched = fa_scheduler_create(fa_time_get_system_prec_clock());
    // fa_scheduler_schedule(sched, sender);
    // fa_scheduler_schedule(sched, sender2);
    // fa_scheduler_loop(sched);

    // for (int i = 0; i < 30; ++i) {
    //     fa_message_send((receiver_t) out_stream, 0, midi(0x90, 48 + i * 2, 100));
    //     fa_thread_sleep(100);
    // }

cleanup:
    // fa_midi_close_stream(stream);
    fa_midi_end_session(session);
}



void test_midi_hotplug()
{
    test_section("Midi hot-plugging");

    midi_session_t session;

    // Begin session
    session = fa_midi_begin_session();

    // Handle possible error
    if (fa_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    fa_midi_add_status_callback(status_changed, string("hello"), session);

    // CFRunLoopRun();
    // fa_thread_sleep(20000);
cleanup:
    // fa_midi_close_stream(stream);
    fa_midi_end_session(session);
}


// ptr_t print_signal(ptr_t data, ptr_t value)
// {
//     fa_print("   %s\n", value);
//     return value;
// }
// ptr_t signal_succ(ptr_t data, ptr_t value)
// {
//     return fa_add(value, i16(1));
// }
// #define ap fa_signal_apply
// #define ap2(f,x,y) ap(ap(f,x),y)
// #define constant(x) fa_signal_constant(x)
// void test_signal()
// {
//     test_section("Signals");
//
//     // fa_signal_t s = fa_signal_constant(i16(1));
//     // fa_signal_t p = fa_signal_constant(i16(10));
//     // fa_signal_t t = fa_signal_apply(fa_signal_identity(), s);
//
//     // fa_signal_t add  = fa_signal_add();
//     // fa_signal_t max  = fa_signal_max();
//     fa_signal_t time = fa_signal_time();
//     fa_signal_t min  = fa_signal_min();
//     // fa_signal_t id   = fa_signal_identity();
//     // fa_signal_t succ = fa_signal_lift(signal_succ, NULL);
//     // fa_signal_t t    = fa_signal_apply(fa_signal_apply(add, s), s);
//
//     fa_signal_run(
//         ap2(min, time, constant(seconds(4))),
//
//         print_signal, NULL);
// }


void test_version()
{
    test_section("Versioning");
    fa_print("%s\n", fa_fa_version());
    fa_print("%s\n", fa_fa_version_string());
}

// --------------------------------------------------------------------------------

static const int  iterations_k = 1;
static const bool stop_k       = false;

// --------------------------------------------------------------------------------

int main(int argc, char const *argv[])
{

    char *bits      = sizeof(void *) == 4 ? "32-bit" : "64-bit";
    printf("Fa %s v%s\n", bits, unstring(fa_fa_version_string()));

    printf("sizeof(fa_ptr_t) = %d\n", (unsigned int) sizeof(fa_ptr_t));
    printf("sizeof(int32_t) = %d\n", (unsigned int) sizeof(int32_t));
    printf("sizeof(int64_t) = %d\n", (unsigned int) sizeof(int64_t));
    printf("sizeof(wchar_t) = %d\n", (unsigned int) sizeof(wchar_t));

    for (int i = 0; i < iterations_k; ++i) {
        if (stop_k) {
            getchar();
        }

        fa_fa_set_log_std();
        // fa_fa_set_log_file(string("/Users/hans/Library/Logs/FAAudio.log"));

        fa_fa_initialize();

        // goto begin;
        test_value_references();
        test_generic_functions();
        test_string();
        test_show();
        test_compare();
        test_rational();
        test_buffer();
        test_time();
        // test_system_time();
        // test_type();
        test_midi_message();

        // test_thread();
        // test_mutex();

        test_atomic();
        test_atomic_queue(5, 2);
        // test_atomic_queue(10, 10);
        // test_atomic_queue(300, 2);
        test_atomic_stack(5, 2);
        // test_atomic_stack(10, 10);
        // test_atomic_stack(300, 2);
        test_atomic_ring_buffer(5, 2);

        test_for_each();
        //test_list();
        test_set();
        test_map();
        // test_graph(string_dappend(fa_system_directory_current(), string("/test/gen.dot")));
        test_priority_queue(10);
        test_json(
            string_dappend(fa_system_directory_current(), string("/test/example.json")));

        test_log();
        test_error();
        test_system_directory();
        test_regex();
        // test_plot(NULL, NULL);
        // test_plot_buffer();
        // test_plot_file(string_dappend(fa_system_directory_current(), string("/test/in.wav")));

        // test_file_stream(
        //     string_dappend(fa_system_directory_current(), string("/test/in.wav")),
        //     string_dappend(fa_system_directory_current(), string("/test/out.wav")));
        // test_buffer_stream();
        // test_audio_stream();
        // test_midi_stream();

        // test_signal();

        // test_version();

// begin:
        // test_midi_hotplug();

// end:
        fa_fa_terminate();
    }

    return 0;
}

