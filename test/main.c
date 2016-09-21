
#include <fa/fa.h>
#include <fa/util.h>

#include <stdio.h>

#include <wchar.h>

void test_section(char *str)
{
    printf("\n\n--------------------\n");
    fa_log_info(fa_string_dappend(fa_string("Running test: "), fa_string_from_utf8(str)));
}

void printhex(const char *s)
{
  while(*s) {
    printf("%02x ", (unsigned char) *s++);
    fflush(stdout);
  }
  printf("\n");
}

void printhex16(const fa_char16_t *s)
{
    while(*s)
      printf("%04x ", (unsigned int) *s++);
    printf("\n");
}

void printwhex(const wchar_t *s) {
    const char* fmt = sizeof(wchar_t) == 4 ? "%08x " : "%04x ";
    while(*s) printf(fmt, *s++);
    printf("\n");
}

// --------------------------------------------------------------------------------

extern char *fa_type_str(fa_ptr_t a);

void test_alloc()
{
    test_section("Allocation");

    for (int i = 0; i < 100; ++i) {
        fa_ptr_t p = fa_malloc(((int)(rand() * 1024.0)) % 1024);
        printf("%p, %d\n", p, ((int) p) % 8);

        assert(fa_is_ref(p));
        assert(!fa_is_double(p));
    }
}
void test_types()
{
    test_section("Types");

    assert(fa_is_int8(fa_i8(123)));
    assert(fa_is_int16(fa_i16(1238)));
    assert(fa_is_int32(fa_i32(1238712)));
    assert(fa_is_int64(fa_i64(1238712)));

    assert(fa_is_float(fa_f32(1238712)));
    assert(fa_is_double(fa_f64(1238712)));
    assert(fa_is_bool(fa_fb(true)));

    assert(fa_is_ref(NULL));
}

void test_value_references()
{
    test_section("Value references");
    // FIXME leaks

    printf("bool:       %s\n", fa_type_str(fa_fb(true)));
    assert(fa_tb(fa_fb(true)) == true);
    assert(fa_tb(fa_fb(false)) == false);

    printf("int8:       %s\n", fa_type_str(fa_i8(62)));
    assert(fa_ti8(fa_i8('h')) == 'h');
    assert(fa_ti8(fa_i8(121)) == 121);
    assert(fa_ti8(fa_i8(-42)) == -42);

    printf("int16:      %s\n", fa_type_str(fa_i16(12372)));
    printf("int16:      %d\n", fa_ti16(fa_i16(1267)));
    assert(fa_ti16(fa_i16(1267)) == 1267);
    assert(fa_ti16(fa_i16(-8712)) == -8712);

    printf("int32:      %s\n", fa_type_str(fa_i32(12372)));
    printf("int32:      %d\n", fa_ti32(fa_i32(1267)));
    assert(fa_ti32(fa_i32(2147483646)) == 2147483646);
    assert(fa_ti32(fa_i32(-343646748)) == -343646748);

    printf("int64:      %s\n", fa_type_str(fa_i64(12372)));
    printf("int64:      %lli\n", fa_ti64(fa_i64(9223372036854775807ll)));
    assert(fa_ti64(fa_i64(4872837827878787871ll)) == 4872837827878787871ll);
    assert(fa_ti64(fa_i64(-6888881236767676711ll)) == -6888881236767676711ll);

    printf("float:      %s\n", fa_type_str(fa_f32(12372)));
    printf("float:      %f\n", fa_tf32(fa_f32(3.141592653589793)));
    assert(fa_tf32(fa_f32(1))   < 1.1);
    assert(fa_tf32(fa_f32(1.1)) > 1);

    printf("double:     %s\n", fa_type_str(fa_f64(12372)));
    printf("double:     %f\n", fa_tf64(fa_f64(3.141592653589793)));
    assert(fa_tf64(fa_f64(1))   < 1.1);
    assert(fa_tf64(fa_f64(1.1)) > 1);
}


// --------------------------------------------------------------------------------

void test_generic_functions()
{
    test_section("Generic functions");
    // TODO leaks

    printf("2 * 3.2                      ==> %f\n",   fa_tf64(fa_multiply(fa_f64(2), fa_f64(3.2))));
    printf("1 / 3                        ==> %f\n",   fa_tf64(fa_divide(fa_f64(1), fa_f64(3))));
    printf("1 + 1.5                      ==> %f\n",   fa_tf64(fa_add(fa_f64(1), fa_f64(1.5))));

    printf("32                  + 1      ==> %i\n",   fa_ti8(fa_add(fa_i8(32), fa_i8(1))));
    printf("5123                + 1      ==> %i\n",   fa_ti16(fa_add(fa_i16(5123), fa_i16(1))));
    printf("2147483646          + 1      ==> %i\n",   fa_ti32(fa_add(fa_i32(2147483646), fa_i32(1))));
    printf("4872837827878787871 + 1      ==> %lli\n", fa_ti64(fa_add(fa_i64(4872837827878787871ll), fa_i64(1))));
    printf("32                  - 1      ==> %i\n",   fa_ti8(fa_subtract(fa_i8(32), fa_i8(1))));
    printf("5123                - 1      ==> %i\n",   fa_ti16(fa_subtract(fa_i16(5123), fa_i16(1))));
    printf("2147483646          - 1      ==> %i\n",   fa_ti32(fa_subtract(fa_i32(2147483646), fa_i32(1))));
    printf("4872837827878787871 - 1      ==> %lli\n", fa_ti64(fa_subtract(fa_i64(4872837827878787871ll), fa_i64(1))));
    printf("3                   / 2      ==> %i\n",   fa_ti8(fa_divide(fa_i8(33), fa_i8(2))));
    printf("3333                / 2      ==> %i\n",   fa_ti16(fa_divide(fa_i16(3333), fa_i16(2))));
    printf("3333333333          / 2      ==> %i\n",   fa_ti32(fa_divide(fa_i32(3333333333l), fa_i32(2))));
    printf("3333333333333333333 / 2      ==> %lli\n", fa_ti64(fa_divide(fa_i64(3333333333333333333ll), fa_i64(2))));
    printf("3                   / 1      ==> %i\n",   fa_ti8(fa_divide(fa_i8(32), fa_i8(1))));

    printf("true == false                ==> %s\n", (fa_equal(fa_fb(true), fa_fb(true))) ? "true" : false);
    printf("32   == 32                   ==> %s\n", (fa_equal(fa_i8(32), fa_i8(32))) ? "true" : false);
    printf("5123 == 5123                 ==> %s\n", (fa_equal(fa_i16(5123), fa_i16(5123))) ? "true" : false);


}



// --------------------------------------------------------------------------------

// static inline void test_string_memdump(void *s, size_t n)
// {
//     for (size_t i = 0; i < n; ++i) {
//         printf("%x ", *((unsigned char *)(s + i)));
//     }
//
//     printf("\n");
// }

void test_string()
{
    test_section("Strings");
    {
        //fa_string_t s = fa_string_single('v');
        //fa_dprint("str: %s\n", s);
    }

    {
        // char* cs = " 新隶体 "; // length 5
        char *cs = "höglund";

        fa_string_t s = fa_string_from_utf8(cs);
        printf("len: %i\n", fa_string_length(s));
        fa_print("str: %s\n", s);

        // printf("charAt 0: %x\n", fa_char_at(0, s));
        // printf("charAt 1: %x\n", fa_char_at(1, s));
        // printf("charAt 2: %x\n", fa_char_at(2, s));
        fa_destroy(s);
    }

    {
        fa_string_t s = fa_string("foo");
        fa_string_t t = fa_string("bar");
        fa_string_t u = fa_string_append(s, t);
        fa_dprint("str: %s\n", s);
        fa_dprint("str: %s\n", t);
        fa_dprint("str: %s\n", u);
    }

    {
        fa_string_t s = fa_string("foo");
        fa_string_t t = fa_string("bar");
        fa_print("str: %s\n", s);
        fa_print("str: %s\n", t);
        {
            fa_string_t u = fa_string_dappend(s, t);
            fa_dprint("str: %s\n", u);
        }
    }
    {
        fa_string_t s = fa_string("Foo, Bar, Baz");
        fa_string_t t = fa_string_copy(s);
        fa_dprint("str: %s\n", s);
        fa_dprint("str: %s\n", t);
    }

    /*
        {
            fa_string_t s = fa_string("FooBarBaz");
            fa_string_t t = fa_string_join_map(apply1, fa_string_single, s);
            fa_dprint("str: %s\n", s);
            fa_dprint("str: %s\n", t);
        }
    */

    {
        fa_string_t s = fa_string("A double quote: \", A backslash: \\");
        fa_dprint("str: %s\n", s);
    }

    {
        //void fa_log_region_count(ch s);
        fa_log_region_count("Before: ");
        fa_string_log_count();
        
        printf("sizeof(wchar_t): %zu  sizeof(fa_char16_t): %zu\n", sizeof(wchar_t), sizeof(fa_char16_t));

        // Chinese characters for "zhongwen" ("Chinese language").
        const char kChineseSampleTextUTF8[] = "\xe4\xb8\xad\xe6\x96\x87"; //{-28, -72, -83, -26, -106, -121, 0};
        fa_char16_t kChineseSampleTextUTF16[] = {0x4E2D, 0x6587, 0};
        fa_string_t chineseText = fa_string_literal(kChineseSampleTextUTF8);

        // Arabic "al-arabiyya" ("Arabic").
        const char kArabicSampleTextUTF8[] = {-40, -89, -39, -124, -40, -71, -40, -79, -40, -88, -39, -118, -40, -87, 0};
        fa_string_t arabicText = fa_string_literal(kArabicSampleTextUTF8);

        // Spanish word "canon" with an "n" with "~" on top and an "o" with an acute accent.
        const char kSpanishSampleTextUTF8[] = {99, 97, -61, -79, -61, -77, 110, 0};
        fa_string_t spanishText = fa_string_literal(kSpanishSampleTextUTF8);

        fa_print("Chinese text: %s\n", chineseText);
        printf("  (length: %d)\n", fa_string_length(chineseText));
        fa_print("Arabic text: %s\n", arabicText);
        fa_print("Spanish text: %s\n", spanishText);

        // printf("utf-8 <-> utf-8: %s\n", (strcmp(kChineseSampleTextUTF8, fa_string_to_utf8(chineseText)) ? "not ok" : "ok"));
        // printf("utf-8 <-> utf-8: %s\n", (strcmp(kArabicSampleTextUTF8, fa_string_to_utf8(arabicText)) ? "not ok" : "ok"));
        // printf("utf-8 <-> utf-8: %s\n", (strcmp(kSpanishSampleTextUTF8, fa_string_to_utf8(spanishText)) ? "not ok" : "ok"));
        //
        printf("Invalid utf-8 return NULL: %s\n", fa_string_from_utf8("Test\xc3\x28test") == 0 ? "ok" : "not ok");
        {
            fa_string_t from_literal = fa_string("Test\xc3\x28test");
            printf("Invalid string literal does not return NULL: %s\n", from_literal ? "ok" : "not ok");
            fa_destroy(from_literal);
        }


        printf("fa_dequal %s\n", fa_dequal(fa_string_from_utf8(kChineseSampleTextUTF8),
            fa_string_from_utf16(kChineseSampleTextUTF16)) ? "ok" : "not ok");

        fa_char16_t* chinese16 = fa_string_to_utf16(chineseText);
        printf("chinese text in utf16 (codepoints): ");
        printhex16(chinese16);
        fa_free(chinese16);

        {
            fa_string_t temp = fa_string_from_utf16(kChineseSampleTextUTF16);
            char* chinese8 = fa_string_to_utf8(temp);
            printf("utf-8 <-> utf-16: %s\n", strcmp((char*)kChineseSampleTextUTF8, chinese8) ? "not ok" : "ok");
            fa_free(chinese8);
            fa_destroy(temp);
        }

        // NB: wchar_t is 32 bit on Mac but 16 bit on Windows!

        //fa_dprint("Wide string: %s\n", fa_string_from_utf16((fa_char16_t*) L"nisse")); // L"\u4E2D\u6587"));

        printf("in (codepoints):  "); printhex16(kChineseSampleTextUTF16);
        printf("in (bytes):       "); printhex((char*) kChineseSampleTextUTF16);
        chinese16 = fa_string_to_utf16(chineseText);
        printf("out (codepoints): "); printhex16(chinese16);
        printf("out (bytes):      "); printhex((char*) chinese16);
        fa_free(chinese16);

        fa_string_log_count();
        fa_destroy(chineseText);
        fa_destroy(arabicText);
        fa_destroy(spanishText);
        fa_string_log_count();

        fa_dprint("Chinese text from utf16: %s\n", fa_string_from_utf16(kChineseSampleTextUTF16));
        //printf("Length of chinese text from utf16: %d\n", fa_string_length(fa_string_from_utf16(kChineseSampleTextUTF16)));

        #ifdef _WIN32
        wchar_t *wide1 = L"a wide string";
        wchar_t *wide2 = fa_string_to_wstr(fa_string("a wide string"));
        printf("Testing wide string: %s\n", wcscmp(wide1, wide2) ? "not ok" : "ok");
        printf("wide1: "); printwhex(wide1);
        printf("wide2: "); printwhex(wide2);
        #endif
        
        
        //void static_string_test();
        //static_string_test();
        
        fa_log_region_count("After: ");
        fa_string_log_count();
    }
    
    // $examples = array(
    //     'Valid ASCII' => "a",
    //     'Valid 2 Octet Sequence' => "\xc3\xb1",
    //     'Invalid 2 Octet Sequence' => "\xc3\x28",
    //     'Invalid Sequence Identifier' => "\xa0\xa1",
    //     'Valid 3 Octet Sequence' => "\xe2\x82\xa1",
    //     'Invalid 3 Octet Sequence (in 2nd Octet)' => "\xe2\x28\xa1",
    //     'Invalid 3 Octet Sequence (in 3rd Octet)' => "\xe2\x82\x28",
    //     'Valid 4 Octet Sequence' => "\xf0\x90\x8c\xbc",
    //     'Invalid 4 Octet Sequence (in 2nd Octet)' => "\xf0\x28\x8c\xbc",
    //     'Invalid 4 Octet Sequence (in 3rd Octet)' => "\xf0\x90\x28\xbc",
    //     'Invalid 4 Octet Sequence (in 4th Octet)' => "\xf0\x28\x8c\x28",
    //     'Valid 5 Octet Sequence (but not Unicode!)' => "\xf8\xa1\xa1\xa1\xa1",
    //     'Valid 6 Octet Sequence (but not Unicode!)' => "\xfc\xa1\xa1\xa1\xa1\xa1",
    // );
}


// --------------------------------------------------------------------------------

void test_show()
{
    test_section("Show");
    fa_print("\n", NULL);
    fa_dprint("%s\n", fa_fb(0));
    fa_dprint("%s\n", fa_i8(129));
    fa_dprint("%s\n", fa_i16(129));
    fa_dprint("%s\n", fa_i32(64000));
    fa_dprint("%s\n", fa_f64(3.1415));
    fa_dprint("%s\n", fa_empty());
    fa_dprint("%s\n", list(fa_i8(1)));
    fa_dprint("%s\n", list(fa_i8(1), fa_i8(2), list(fa_i8(1), fa_i8(2), fa_fb(true))));
    fa_dprint("%s\n", list(
                  fa_pair_create(fa_string("hans"), fa_string("höglund")),
                  fa_pair_create(fa_string("lisa"), fa_string("streich")),
                  fa_pair_create(fa_string("mats"), fa_string("erlandsson"))));
}


// --------------------------------------------------------------------------------

void test_compare()
{
    test_section("Comparison");
    fa_dprint("\"abc\" <  \"abd\"               ==> %s\n", fa_fb(fa_less_than(fa_string("abc"), fa_string("abd"))));
    fa_dprint("\"abc\" <= \"abd\"               ==> %s\n", fa_fb(fa_less_than_equal(fa_string("abc"), fa_string("abd"))));
    fa_dprint("\"abc\" >  \"abd\"               ==> %s\n", fa_fb(fa_greater_than(fa_string("abc"), fa_string("abd"))));
    fa_dprint("\"abc\" >= \"abd\"               ==> %s\n", fa_fb(fa_less_than_equal(fa_string("abc"), fa_string("abd"))));
}


// --------------------------------------------------------------------------------

void test_rational()
{
    test_section("Rational numbers");
    fa_dprint("1/3 <  1/2                   ==> %s\n", fa_fb(fa_less_than(fa_ratio(1, 3), fa_ratio(1, 2))));
    fa_dprint("1/3 >  1/2                   ==> %s\n", fa_fb(fa_greater_than(fa_ratio(1, 3), fa_ratio(1, 2))));
    fa_dprint("1/3 == 2/6                   ==> %s\n", fa_fb(fa_equal(fa_ratio(1, 3), fa_ratio(2, 6))));
    fa_dprint("1/3 == 254/762               ==> %s\n", fa_fb(fa_equal(fa_ratio(1, 3), fa_ratio(254, 762))));
    fa_dprint("1/3 <= 7/8                   ==> %s\n", fa_fb(fa_equal(fa_ratio(1, 3), fa_ratio(254, 762))));
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
        fa_print("size(b)                      ==> %s\n", fa_i32(fa_buffer_size(b)));
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
        fa_print("size(b)                      ==> %s\n", fa_i32(fa_buffer_size(b)));
        fa_destroy(b);
    }
}


// --------------------------------------------------------------------------------

void test_buffer_zip_unzip()
{
    test_section("Buffer zip/unzip");

    {
        fa_buffer_t b = fa_buffer_create(4);
        fa_buffer_set(b, 0, 'a');
        fa_buffer_set(b, 1, 'b');
        fa_buffer_set(b, 2, 'c');
        fa_buffer_set(b, 3, 'd');
        fa_print("b                            ==> %s\n", b);

        fa_unpair(fa_buffer_unzip(b), b1, b2)
        {
            fa_print("b1 (where b1,b2 = unzip b)   ==> %s\n", b1);
            fa_print("b2 (where b1,b2 = unzip b)   ==> %s\n", b2);
        }
    }


    {
        fa_buffer_t b = fa_buffer_create(3);
        fa_buffer_set(b, 0, 'a');
        fa_buffer_set(b, 1, 'b');
        fa_buffer_set(b, 2, 'c');
        fa_print("b                            ==> %s\n", b);

        fa_unpair(fa_buffer_unzip(b), b1, b2)
        {
            fa_print("b1 (where b1,b2 = unzip b)   ==> %s\n", b1);
            fa_print("b2 (where b1,b2 = unzip b)   ==> %s\n", b2);
        }
    }
}


// --------------------------------------------------------------------------------


void test_buffer_meta()
{
    test_section("Buffer meta");

    {
        fa_buffer_t b = fa_buffer_create(1);

        fa_print("fa_buffer_meta(b)            ==> %s\n", fa_buffer_meta(b));
        fa_buffer_set_meta(b, fa_string("test"), fa_from_float(1.23456));
        fa_print("fa_buffer_meta(b)            ==> %s\n", fa_buffer_meta(b));
        fa_buffer_set_meta(b, fa_string("test"), fa_from_float(7.89012));
        fa_print("fa_buffer_meta(b)            ==> %s\n", fa_buffer_meta(b));

        fa_destroy(b);
    }
    // exit(-1);
}


// --------------------------------------------------------------------------------

void test_time()
{
    test_section("Time");

    fa_time_t t = fa_time_create(1, 0, 0, fa_ratio(25, 8));
    fa_time_t u = fa_time_create(0, 1, 1, fa_ratio(58, 1));

    fa_print("t                            ==> %s\n", t);
    fa_print("u                            ==> %s\n", u);
    fa_print("t + u                        ==> %s\n", fa_add(t, u));

    fa_print("fa_time_to_iso(t)       ==> %s\n", fa_time_to_iso(t));
    fa_print("fa_time_to_iso(u)       ==> %s\n", fa_time_to_iso(u));


    fa_print("200 ms       ==> %s\n", fa_milliseconds(200));
    fa_print("200 ms       ==> %s\n", fa_time_create(0, 0, 0, fa_ratio(1, 5)));

    fa_destroy(t);
    fa_destroy(u);
}


// --------------------------------------------------------------------------------

// void test_system_time()
// {
//     test_section("System time");
//
//     fa_clock_t system_clock = fa_time_get_system_prec_clock();
//
//     for (int i = 0; i < 10; ++i) {
//         // fa_print("system()                     ==> %s\n", fa_time_system());
//         // fa_print("cpu()                        ==> %s\n", fa_time_cpu());
//         // fa_print("system()                     ==> %s\n", fa_time_from_system(fa_time_system()));
//         // fa_print("cpu()                        ==> %s\n", fa_time_from_cpu(fa_time_cpu()));
//
//         fa_print("time(systemClock)            ==> %s\n", fa_time_time(system_clock));
//         fa_print("ticks(systemClock)           ==> %s\n", fa_i64(fa_time_ticks(system_clock)));
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
//     fa_dprint("type(fa_i8)                  ==> %s\n", type(fa_i8));
//     // fa_dprint("size_of(1024,type(fa_i8))    ==> %s\n", fa_i32(fa_type_size_of(1024, type(fa_i8))));
//     // fa_dprint("align_of(1024,type(fa_i8))   ==> %s\n", fa_i32(fa_type_align_of(type(fa_i8))));
//     printf("\n");
//
//     // fa_dprint("type(fa_f64)                 ==> %s\n", type(fa_f64));
//     // fa_dprint("size_of(1024,type(fa_f64))   ==> %s\n", fa_i32(fa_type_size_of(1024, type(fa_f64))));
//     // fa_dprint("align_of(1024,type(fa_f64))  ==> %s\n", fa_i32(fa_type_align_of(type(fa_f64))));
//     // printf("\n");
//     //
//     // fa_type_t t = type_pair(type(fa_i8), type(fa_f64));
//     // fa_dprint("t                            ==> %s\n", t);
//     // fa_dprint("size_of(1024,t)              ==> %s\n", fa_i32(fa_type_size_of(1024, t)));
//     // fa_dprint("align_of(1024,t)             ==> %s\n", fa_i32(fa_type_align_of(t)));
//     // printf("\n");
//     //
//     // fa_type_t u = type_pair(type_vector(type(fa_i8), 10), type(fa_f64));
//     // fa_dprint("u                            ==> %s\n", u);
//     // fa_dprint("size_of(1024,u)              ==> %s\n", fa_i32(fa_type_size_of(1024, u)));
//     // fa_dprint("align_of(1024,u)             ==> %s\n", fa_i32(fa_type_align_of(u)));
//     // printf("\n");
//     //
//     // fa_type_t u2 = type_pair(type_frame(type(fa_i8)), type(fa_f64));
//     // fa_dprint("u2                           ==> %s\n", u2);
//     // fa_dprint("size_of(1024,u2)             ==> %s\n", fa_i32(fa_type_size_of(1024, u2)));
//     // fa_dprint("align_of(1024,u2)            ==> %s\n", fa_i32(fa_type_align_of(u2)));
//     // printf("\n");
//
//     fa_type_t v = type_pair(type(fa_i8), type_pair(type(fa_i8), type_pair(type(fa_i8),
//                                              type_pair(type(fa_i8), type_pair(type(fa_i8), type_pair(type(fa_i8),
//                                                        type_pair(type(fa_i8), type_pair(type(fa_i8), type_pair(type(fa_i8),
//                                                                type(fa_i8))))))))));
//
//     fa_print("v                            ==> %s\n", v);
//     // fa_dprint("size_of(1024,v)              ==> %s\n", fa_i32(fa_type_size_of(1024, v)));
//     // fa_dprint("align_of(1024,v)             ==> %s\n", fa_i32(fa_type_align_of(v)));
//     fa_destroy(v);
//     printf("\n");
// }



// --------------------------------------------------------------------------------

fa_ptr_t test_atomic_add10(fa_ptr_t x, fa_ptr_t _)
{
    return (fa_ptr_t)((intptr_t) x + 10);
}

void test_atomic()
{
    test_section("Atomic");

    // treat as integer
    {
        fa_atomic_t a = fa_atomic_create();
        fa_print("a                            ==> %s\n", a);

        fa_atomic_set(a, (fa_ptr_t) 0x5);
        fa_print("a                            ==> %s\n", a);

        fa_atomic_modify(a, test_atomic_add10, 0);
        fa_print("a                            ==> %s\n", a);

        // fa_atomic_add(a, (fa_ptr_t) - 0xf);
        // fa_print("a                            ==> %s\n", a);

        fa_atomic_exchange(a, (fa_ptr_t) 1, (fa_ptr_t) 0xfe);
        fa_print("a                            ==> %s\n", a); // fails, still 0

        fa_atomic_exchange(a, (fa_ptr_t) 0, (fa_ptr_t) 0xff);
        fa_print("a                            ==> %s\n", a); // now ff
    }
}


// --------------------------------------------------------------------------------

struct test_atomic_queue_reader_args {
    fa_atomic_queue_t queue;
    fa_atomic_t active;
};

fa_ptr_t test_atomic_queue_reader(fa_ptr_t x)
{
    struct test_atomic_queue_reader_args *args = x;
    fa_atomic_queue_t q = args->queue;
    fa_atomic_t               a = args->active;
    fa_ptr_t                  v;

    while (true) {
        if (!fa_tb(fa_atomic_get(a))) {
            return v;
        }

        if ((v = fa_atomic_queue_read(q))) {
            printf("         |- %5d    \n", fa_ti32(v));
        }

        fa_thread_sleep(rand() % 100);
    }
}

void test_atomic_queue_(int iter, long sleepTime)
{
    test_section("Atomic queues");
    {
        fa_atomic_queue_t q = fa_atomic_queue_create();

        struct test_atomic_queue_reader_args args = { q, fa_atomic() };
        fa_atomic_set(args.active, fa_fb(true));

        fa_thread_t t = fa_thread_create(test_atomic_queue_reader, &args, NULL);

        fa_print("q                            ==> %s\n", q);

        for (int i = 0; i < iter; ++i) {
            fa_thread_sleep(rand() % 100);
            fa_atomic_queue_write(q, fa_i32(i));
            printf("  %5d -|  \n", i);
        }

        fa_thread_sleep(sleepTime);
        fa_atomic_set(args.active, fa_fb(false));
        fa_thread_join(t); // TODO how to kill?
        fa_destroy(q);
    }
}

void test_atomic_queue()
{
    test_atomic_queue_(5, 2);
    test_atomic_queue_(10, 10);
    test_atomic_queue_(50, 2);
}


// --------------------------------------------------------------------------------

/*
struct test_atomic_stack_reader_args {
    fa_atomic_stack_t stack;
    fa_atomic_t active;
};

fa_ptr_t test_atomic_stack_reader(fa_ptr_t x)
{
    struct test_atomic_stack_reader_args *args = x;
    fa_atomic_stack_t q = args->stack;
    fa_atomic_t               a = args->active;
    fa_ptr_t                  v;

    while (true) {
        if (!fa_tb(fa_atomic_get(a))) {
            return v;
        }

        if ((v = fa_atomic_stack_read(q))) {
            printf("         |- %5d    \n", fa_ti32(v));
        }

        srand(time(NULL));
        fa_thread_sleep(rand() % 100);
    }
}

void test_atomic_stack_(int iter, long sleepTime)
{
    test_section("Atomic stacks");
    {
        fa_atomic_stack_t q = fa_atomic_stack_create();

        struct test_atomic_stack_reader_args args = { q, fa_atomic() };
        fa_atomic_set(args.active, fa_fb(true));

        fa_thread_t t = fa_thread_create(test_atomic_stack_reader, &args);

        fa_print("q                            ==> %s\n", q);

        for (int i = 0; i < iter; ++i) {
            if (i % 10) {
                fa_thread_sleep(rand() % 100);
            }

            fa_atomic_stack_write(q, fa_i32(i));
            printf("  %5d -|  \n", i);
        }

        fa_thread_sleep(sleepTime);
        fa_atomic_set(args.active, fa_fb(false));
        fa_thread_join(t);
        fa_destroy(q);
    }
}

void test_atomic_stack()
{
    test_atomic_stack_(5, 2);
}
*/


// --------------------------------------------------------------------------------

// TODO move
typedef fa_atomic_ring_buffer_t ring_fa_buffer_t;
#define ring_buffer(size) fa_atomic_ring_buffer_create(size)

struct test_atomic_ring_buffer_reader_args {
    fa_atomic_ring_buffer_t ring_buffer;
    fa_atomic_t active;
};

fa_ptr_t ring_buffer_reader(fa_ptr_t x)
{
    struct test_atomic_ring_buffer_reader_args *args = x;
    fa_atomic_ring_buffer_t q = args->ring_buffer;
    fa_atomic_t                a = args->active;
    char                    v;

    fa_thread_sleep(1000);

    while (true) {
        if (!fa_tb(fa_atomic_get(a))) {
            return NULL;
        }

        if (fa_atomic_ring_buffer_read(q, (uint8_t *) &v)) {
            printf("         |- %5d    \n", v);
        }

        srand(time(NULL));
        fa_thread_sleep(rand() % 100);
    }
}

void test_atomic_ring_buffer_(int iter, long sleepTime)
{
    test_section("RingBuffer");
    {
        fa_atomic_ring_buffer_t q = fa_atomic_ring_buffer_create(1024);

        struct test_atomic_ring_buffer_reader_args args = { q, fa_atomic() };
        fa_atomic_set(args.active, fa_fb(true));

        fa_thread_t t = fa_thread_create(ring_buffer_reader, &args, NULL);

        fa_print("q                            ==> %s\n", q);

        for (int i = 0; i < iter; ++i) {
            if (i % 10) {
                fa_thread_sleep(rand() % 100);
            }

            fa_atomic_ring_buffer_write(q, i);
            printf("  %5d -|  \n", i);
        }

        fa_thread_sleep(sleepTime);
        fa_atomic_set(args.active, fa_fb(false));
        fa_thread_join(t);
        fa_destroy(q);
    }
}

void test_atomic_ring_buffer()
{
    test_atomic_ring_buffer_(5, 2);
}

// --------------------------------------------------------------------------------

fa_ptr_t test_thread_printer(fa_ptr_t data)
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
    t  = fa_thread_create(test_thread_printer, (fa_ptr_t) 10, NULL);
    t2 = fa_thread_create(test_thread_printer, (fa_ptr_t) 11, NULL);

    fa_thread_sleep(1000);
    fa_thread_join(t);
    fa_thread_join(t2);
}


// --------------------------------------------------------------------------------

typedef struct {
    fa_thread_mutex_t mut;
    int val;
} test_mutex_lock_index;

fa_ptr_t test_mutex_locker(fa_ptr_t x)
{
    test_mutex_lock_index *i = (test_mutex_lock_index *) x;

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
        test_mutex_lock_index i = { m, j };
        fa_thread_t t = fa_thread_create(test_mutex_locker, (fa_ptr_t) &i, NULL);
        fa_thread_sleep(100);
        fa_thread_detach(t);
    }

    fa_thread_sleep(1200);
}


// --------------------------------------------------------------------------------

void test_for_each()
{
    test_section("For each loops");

    fa_let(x, 33) {
        fa_let(y, 1)
        fa_let(z, x + y)
        fa_print("%s\n", fa_i32(z));
    }

    printf("\n");
    fa_with(list, list(fa_i32(1), fa_i32(2), fa_i32(3), fa_i32(4)), fa_destroy(list)) {
        fa_for_each(x, list) {
            fa_print(">    %s\n", x);
        }
    }

    printf("\n");
    fa_with(set, set(fa_i32(1), fa_i32(1), fa_i32(2), fa_i32(1)), fa_destroy(set)) {
        fa_for_each(x, fa_set_to_list(set)) {
            fa_print(">    %s\n", x);
        }
    }

    printf("\n");

    // fa_with(map, map(
    //             fa_string("foo"), fa_i16(1),
    //             fa_string("bar"), list(fa_i16(1), fa_i16(2), fa_i16(3))),
    //         fa_destroy(map)) {
    //     fa_for_each(x, fa_map_to_list(map)) {
    //         fa_print(">    %s\n", x);
    //     }
    // }
}



// --------------------------------------------------------------------------------

bool test_list_is_even16(fa_ptr_t data, fa_ptr_t p)
{
    return fa_ti16(p) % 2 == 0;
}

bool test_list_is_odd16(fa_ptr_t data, fa_ptr_t p)
{
    return fa_ti16(p) % 2 != 0;
}

fa_ptr_t times2(fa_ptr_t data, fa_ptr_t p)
{
    return fa_i16(2 * fa_ti16(p));
}

fa_ptr_t times10(fa_ptr_t data, fa_ptr_t p)
{
    return fa_i16(10 * fa_ti16(p));
}

// x = [x,x]
fa_ptr_t dup_list(fa_ptr_t ct, fa_ptr_t x)
{
    return list(x, x);
}

void test_list()
{
    test_section("List");

    {
        printf("\n");

        fa_list_t as = fa_empty();
        fa_print("fa_empty()                      ==> %s\n", as);
        fa_destroy(as);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_list_t bs = fa_list_cons(fa_i16(0), as);

        fa_print("as                           ==> %s\n", as);
        fa_print("cons(0,as)                   ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_list_t bs = fa_list_append(as, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("append(as,as)                ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_list_t bs = fa_list_copy(as);
        fa_print("as                           ==> %s\n", as);
        fa_print("copy(as)                     ==> %s\n", bs);
        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_list_t bs = fa_list_init(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("init(as)                     ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_ptr_t v = fa_list_last(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("last(as)                     ==> %s\n", v);

        fa_destroy(as);
        fa_destroy(v);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3));

        fa_print("as                           ==> %s\n", as);
        fa_print("length(as)                   ==> %s\n", fa_i16(fa_list_length(as)));

        fa_destroy(as);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_reverse(as);

        fa_print("as                           ==> %s\n", as);
        fa_print("reverse(as)                  ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    {
        printf("\n");

        // fa_list_t as = list(fa_i16(1), fa_i16(-2), fa_i16(0), fa_i16(4), fa_i16(123));
        fa_list_t as = fa_list_enumerate(0, 10);
        as = fa_list_reverse(as);
        as = fa_list_dmap(apply1, fa_i32, as);

        fa_list_t bs = fa_list_sort(as, fa_less_than);

        fa_print("as                           ==> %s\n", as);
        fa_print("sort(as)                     ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_take(3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("take(3,as)                   ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_drop(3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("drop(3,as)                   ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_ptr_t v = fa_list_index(1, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("index(1,as)                  ==> %s\n", v);

        fa_destroy(as);
    }

    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_range(1, 3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("range(1,3,as)                ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_remove_range(1, 3, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("removeRange(1,3,as)          ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }

    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t xs = list(fa_i16(0), fa_i16(0));
        fa_list_t bs = fa_list_insert_range(2, xs, as);

        fa_print("as                           ==> %s\n", as);
        fa_print("insertRange(2,list(0,0),as)  ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(xs);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_insert(2, fa_i16(0), as);

        fa_print("as                           ==> %s\n", as);
        fa_print("insert(2,0,as)               ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_insert(0, fa_i16(0), as);

        fa_print("as                           ==> %s\n", as);
        fa_print("insert(0,1,as)               ==> %s\n", bs);

        fa_destroy(as);
        fa_destroy(bs);
    }
    {
        printf("\n");

        fa_list_t as = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t bs = fa_list_remove(2, as);

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

        fa_list_t as = list(fa_i16(1), fa_i16(3), fa_i16(5));

        fa_print("as                           ==> %s\n", as);
        fa_print("indexOf(0,as)                ==> %s\n", fa_i16(fa_list_index_of(fa_i16(0), as)));
        fa_print("indexOf(1,as)                ==> %s\n", fa_i16(fa_list_index_of(fa_i16(1), as)));
        fa_print("indexOf(2,as)                ==> %s\n", fa_i16(fa_list_index_of(fa_i16(2), as)));
        fa_print("indexOf(3,as)                ==> %s\n", fa_i16(fa_list_index_of(fa_i16(3), as)));
        fa_print("indexOf(4,as)                ==> %s\n", fa_i16(fa_list_index_of(fa_i16(4), as)));
        fa_print("indexOf(5,as)                ==> %s\n", fa_i16(fa_list_index_of(fa_i16(5), as)));

        fa_destroy(as);
    }
    {
        printf("\n");

        fa_list_t xs = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t ys = fa_list_filter(test_list_is_odd16, 0, xs);

        fa_print("xs                           ==> %s\n", xs);
        fa_print("filter(test_list_is_odd,ys)            ==> %s\n", ys);

        fa_destroy(xs);
        fa_destroy(ys);
    }

    {
        printf("\n");

        fa_list_t xs = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t ys = fa_list_map(times10, 0, xs);

        fa_print("xs                           ==> %s\n", xs);
        fa_print("map(times10,ys)              ==> %s\n", ys);

        fa_destroy(xs);
        fa_destroy(ys);
    }

    {
        printf("\n");

        fa_list_t xs = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t ys = list(fa_i16(66), fa_i16(77));
        fa_list_t zss = list(xs, ys, xs);

        fa_print("[xs,ys]                      ==> %s\n", zss);
        fa_print("join([xs,ys])                ==> %s\n", fa_list_join(zss));

        fa_destroy(xs);
        fa_destroy(ys);
    }


    {
        printf("\n");

        fa_list_t xs = list(fa_i16(1), fa_i16(2), fa_i16(3), fa_i16(4), fa_i16(5));
        fa_list_t ys = fa_list_join_map(dup_list, 0, xs);

        fa_print("xs                           ==> %s\n", xs);
        fa_print("joinMap(\\x -> [x,x])         ==> %s\n", ys);

        fa_destroy(xs);
        fa_destroy(ys);
    }


    {
        printf("\n");

        fa_list_t xs = fa_list_enumerate(0, 50);

        xs = fa_list_dreverse(xs);
        // fa_print("reverse(xs)                  ==> %s\n", xs);

        xs = fa_list_dmap(apply1, fa_i16, xs);
        // fa_print("xs                           ==> %s\n", xs);

        xs = fa_list_dfilter(test_list_is_odd16, 0, xs);
        // fa_print("filter(test_list_is_odd,xs)            ==> %s\n", xs);

        xs = fa_list_dmap(times10, 0, xs);
        // fa_print("map(times10, xs)             ==> %s\n", xs);

        fa_destroy(xs);
    }
    {
        printf("\n");

        fa_list_t xs = fa_list_enumerate(0, 12);
        xs = fa_list_dmap(apply1, fa_i8, xs);

        fa_print("xs                           ==> %s\n", xs);
        fa_ptr_t sum = fa_list_dfold_left(apply2, fa_add, fa_i8(0), xs);
        fa_print("sum(xs)                      ==> %s\n", sum);
    }
    {
        fa_list_t xs = fa_empty();
        xs = fa_list_dreverse(xs);
        fa_dprint("Reversed empty list:         ==> %s\n ", xs);
    }
}


// --------------------------------------------------------------------------------

void test_set()
{
    test_section("Set");
    {
        printf("\n");

        fa_set_t a = set(fa_i16(1), fa_i16(3), fa_i16(2));

        a = fa_set_dadd(fa_i16(1), a);
        a = fa_set_dadd(fa_i16(5), a);
        a = fa_set_dadd(fa_i16(3), a);
        a = fa_set_dremove(fa_i16(3), a);

        fa_print("a                            ==> %s\n", a);
        fa_print("size(a)                      ==> %s\n", fa_i16(fa_set_size(a)));
        fa_destroy(a);
    }

    {
        printf("\n");

        fa_set_t a = set(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_set_t b = set(fa_i16(3), fa_i16(4));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a + b                        ==> %s\n", fa_set_sum(a, b));

        fa_destroy(a);
        fa_destroy(b);
    }

    {
        printf("\n");

        fa_set_t a = set(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_set_t b = set(fa_i16(3), fa_i16(4));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a - b                        ==> %s\n", fa_set_difference(a, b));

        fa_destroy(a);
        fa_destroy(b);
    }

    {
        printf("\n");

        fa_set_t a = set(fa_i16(1), fa_i16(2), fa_i16(3));
        fa_set_t b = set(fa_i16(3), fa_i16(4));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a x b                        ==> %s\n", fa_set_product(a, b));

        fa_destroy(a);
        fa_destroy(b);
    }

    {
        printf("\n");

        fa_set_t a = set(fa_string("foo"), fa_string("bar"));
        fa_set_t b = set(fa_string("hi"), fa_string("ho"));
        fa_set_t c = set(fa_i16(0), fa_i16(1));

        fa_print("a                            ==> %s\n", a);
        fa_print("b                            ==> %s\n", b);
        fa_dprint("a x b                        ==> %s\n", fa_set_product(a, b));

        fa_set_t ab = fa_set_product(a, b);
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

        fa_map_t a = fa_map_empty();

        a = fa_map_dadd(fa_string("name"), fa_string("Hans"), a);
        a = fa_map_dset(fa_string("name"), fa_string("Sven"), a);
        a = fa_map_dset(fa_string("age"), fa_i16(22), a);
        a = fa_map_dset(fa_string("age"), fa_i16(22), a);

        // a = fa_map_dremove(fa_string("age"), a);
        // a = fa_map_dadd(fa_string("age"), fa_i16(25), a);
        a = fa_map_dset(fa_string("skills"), list(fa_string("programming"), fa_string("composition")), a);

        // a = fa_map_dadd(fa_string("happy"), fa_fb(true), a);
        // a = fa_map_dadd(fa_string("pair"), pair(fa_fb(true), fa_f64(3.1415)), a);
        // a = fa_map_dadd(fa_string("ratio"), fa_ratio(1, 3), a);
        // a = fa_map_dadd(fa_string("ratio2"), fa_multiply(fa_ratio(4, 4444), fa_ratio(1, 2)), a);

        fa_print("a                            ==> %s\n", a);
        fa_print("size(a)                      ==> %s\n", fa_i16(fa_map_size(a)));

        fa_print("a.name                       ==> %s\n", fa_map_get(fa_string("name"), a));
        fa_print("a.age                        ==> %s\n", fa_map_get(fa_string("age"), a));
        fa_print("a.skills                     ==> %s\n", fa_map_get(fa_string("skills"), a));
        fa_print("a.happy                      ==> %s\n", fa_map_get(fa_string("happy"), a));
        fa_print("a.pair                       ==> %s\n", fa_map_get(fa_string("pair"), a));

        fa_destroy(a);
    }
}


// --------------------------------------------------------------------------------

// void test_graph(fa_string_t path)
// {
//     test_section("Graph");
//     {
//         fa_graph_t a = fa_graph_empty();
//
//
//         a = fa_graph_insert(fa_string("foo"), a);
//         a = fa_graph_connect(fa_string("foo"), fa_string("foo"), fa_string("(1)"), a);
//
//         // a = fa_graph_insert(pair(fa_string("a"), fa_string("b")), a);
//         // a = fa_graph_connect(
//         //     pair(fa_string("a"), fa_string("b")),
//         //     pair(fa_string("a"), fa_string("b")),
//         //     fa_string("(1)"), a);
//
//         fa_print("a                            ==> %s\n", a);
//         fa_system_directory_write_file(path, fa_graph_to_dot(
//                                                 fa_string("#include \"doc/graphs/header.dot\""),
//                                                 fa_string(""),
//                                                 a));
//     }
// }


// --------------------------------------------------------------------------------

void test_priority_queue(int iter)
{
    test_section("Priority queue");

    fa_priority_queue_t q = fa_priority_queue_empty();
    srand(time(NULL));

    for (int i = 0; i < iter; ++i) {
        fa_priority_queue_insert(fa_add(fa_hours(rand() % 24), fa_seconds(rand() % 3600)), q);
    }

    while (fa_priority_queue_peek(q)) {
        fa_dprint("     -> %s \n", fa_priority_queue_pop(q));
    }

}


// --------------------------------------------------------------------------------

void test_json(fa_string_t path)
{
    extern void fa_puts(fa_string_t string);

    test_section("JSON conversion");

    fa_string_t json = fa_system_directory_read_file(path);
    // printf("%s\n", fa_unstring(json));

    fa_ptr_t data = fa_string_from_json(json);
    fa_print("data                         ==> %s\n", data);

    fa_string_t json2 = fa_string_to_json(data);
    fa_puts(json2);

    fa_puts(fa_string_to_json(
                fa_pair_create(fa_i32(1), fa_i32(2))));

    fa_puts(fa_string_to_json(
                list(fa_pair_create(fa_i32(1), fa_i32(2)), fa_pair_create(fa_i32(3), fa_i32(4)))));

    fa_puts(fa_string_to_json(
                set(fa_pair_create(fa_i32(1), fa_i32(2)), fa_pair_create(fa_i32(1), fa_i32(2)))));

    fa_puts(fa_string_to_json(
                map(
                    fa_string("foo"), fa_i32(1),
                    fa_string("bar"), list(fa_i32(1), fa_i32(2), fa_i32(3)))));

}



// --------------------------------------------------------------------------------

// void test_dispatcher()
// {
//     test_section("Dispatcher");
//
//     fa_dispatcher_t disp = lockfree_dispatcher();
//
//     fa_ptr_t val = map(
//                     fa_string("lyrics"), list(fa_string("Help"), fa_string("me"), fa_string("if"), fa_string("you"), fa_string("can")),
//                     fa_string("pitches"), list(fa_ratio(60, 1), fa_ratio(62, 1))
//                 );
//
//     fa_message_send((fa_receiver_t) disp, fa_i16(1), val);
//     fa_message_send((fa_receiver_t) disp, fa_i16(2), fa_string("World!"));
//     fa_message_send((fa_receiver_t) disp, fa_i16(2), fa_string("World!"));
//     fa_message_send((fa_receiver_t) disp, fa_i16(2), fa_string("World!"));
//     fa_message_send((fa_receiver_t) disp, fa_i16(2), fa_string("World!"));
//
//     fa_list_t msgs = fa_list_empty();
//
//     while (true) {
//         fa_message_sync((fa_sender_t) disp);
//         msgs = fa_message_receive((fa_sender_t) disp, fa_i16(1));
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
//     fa_message_fa_sender_t s =
//         fa_system_event_receive(
//             list(
//                 // fa_i16(mouse_move_event)
//                 // fa_i16(mouse_down_event)
//                 fa_i16(key_down_event),
//                 fa_i16(key_up_event)
//
//             ));
//     fa_message_fa_receiver_t r =
//         fa_system_event_send_std();
//
//     for (int i = 0; i < 100000; ++i) {
//         fa_message_sync(s);
//         fa_for_each(x, fa_message_receive(s, fa_i16(0))) {
//             // fa_print("    Received: %s\n", x);
//             fa_message_send(r, fa_i16(0), x);
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
//         // fa_event_t ha = now(fa_string("höglund"));
//         // fa_event_t ho = now(fa_string("holmgren"));
//
//         // fa_event_t a = merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha,
//             // delay_event(milliseconds(200*2), merge_event(ha, never())))))))))));
//
//         // fa_event_t b = merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho,
//         //     delay_event(milliseconds(240*2), merge_event(ho, never())))))))))));
//
//         // fa_event_t s1 = fa_event_later(seconds(1), NULL);
//         // fa_event_t s3 = fa_event_later(seconds(3), NULL);
//
//         fa_event_t mm = fa_system_event_mouse_move();
//         // fa_event_t md = fa_system_event_mouse_down();
//         // fa_event_t mu = fa_system_event_mouse_up();
//         // fa_event_t kd = fa_system_event_key_down();
//         // fa_event_t ku = fa_system_event_key_up();
//         // fa_event_t mouseX = fa_event_map(apply1, fa_pair_first, mm);
//         // fa_event_t mouseY = fa_event_map(apply1, fa_pair_second, mm);
//
//         // fa_event_t y2 = merge_event(switch_event(kd, merge_event(a, mm), merge_event(b, md)), later(seconds(5), list(fa_string("flux"))));
//         // fa_event_t y2 = switch_event(ku, switch_event(kd,never(),mm), merge_event(delay_event(seconds(3),b),md));
//         // fa_event_t y2 = switch_event(kd,mm,merge_event(md,mu));
//         // fa_event_t y2 = fa_event_filter(fa_less_than, fa_f64(500), mouseX);
//         fa_event_t y2 = mm;
//         // fa_print("The event: %s\n", mouseX);
//         fa_event_t z  = fa_system_event_write_std(y2);
//
//         {
//             fa_clock_t     clk = fa_time_get_system_prec_clock();
//             fa_scheduler_t sched = fa_scheduler_create(clk);
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

// fa_ptr_t add1234(fa_ptr_t c, fa_ptr_t x)
// {
// return fa_i8(fa_ti8(x) + 1234);
// }
//
// void test_processor_graphs(fa_string_t path)
// {
//     test_section("Processors");
//
//     inform(fa_string_append(fa_string("Writing "), path));
//
//     processor_t p, q, chain, rchain;
//     p = unary(type(fa_i8), type(fa_i8), add1234, NULL);
//     chain = seq(p, seq(p, seq(p, seq(p, seq(p, p)))));
//     rchain = seq(seq(seq(seq(p, p), p), p), p);
//     q =
//         seq(split(type(fa_i8)),
//
//             par(
//                 seq(seq(seq(p, p), p), p),
//
//                 seq(
//                     split(type(fa_i8)),
//                     par(seq(split(type(fa_i8)), par(chain, seq(split(type(fa_i8)), par(chain, chain)))),
//                         seq(split(type(fa_i8)), par(p, seq(p, p))))
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

// fa_ptr_t cont(fa_ptr_t x)
// {
//     printf("Continuing...\n");
//     return x;
// }
//
// double f1(void *ct, int i, double t, double x)
// {
//     double pi  = 3.141592653589793;
//     double tau = 2 * pi;
//     double t0  = x;
//     // double t2  = t + x;
//     // t2 = t2;
//
// #define step(p) ((float)((int)fmod(t,p)%p))/p
//
//     switch (i) {
//     case 3:
//         return step(5);
//
//     case 2:
//         return -0.5 * cos(tau * t0 * 0.5 + pi);
//
//     case 1:
//         return  0.5 * cos(tau * t0 * 0.5 + pi);
//
//     case 0:
//         return  0.5 * cos(tau * t0 * 0.5 + pi) * sin(tau * t0 * 3);
//
//     default:
//         return 0;
//     }
// }
//
// void test_plot()
// {
//     test_section("Plot");
//     fa_plot_continous(f1, NULL, NULL, NULL);
// }


// --------------------------------------------------------------------------------

// void test_plot_buffer()
// {
//     fa_buffer_t buf = fa_buffer_create(44100 * sizeof(double));
//
//     for (int i = 0; i < 44100; ++i) {
//         double r = (double) rand() / RAND_MAX;
//         double x = (double) i / 44100;
//         fa_buffer_set_double(buf, i, (r * 2 - 1) * sin(x * 10));
//     }
//
//     fa_plot_buffer_double(buf, NULL, NULL);
// }


// --------------------------------------------------------------------------------

// void test_plot_file(fa_string_t path)
// {
//     test_section("Plot file");
//
//     fa_pair_t res = fa_buffer_read_audio(path);
//
//     if (fa_error_check(res)) {
//         fa_error_log(NULL, (fa_error_t) res);
//         return;
//     }
//
//     fa_print("%s\n", res);
//
//     fa_buffer_t buf = fa_pair_second(res);
//     fa_plot_buffer_double(buf, NULL, NULL);
//     fa_destroy(buf);
//     fa_destroy(res);
// }


// --------------------------------------------------------------------------------

void test_error()
{

}


// --------------------------------------------------------------------------------

void test_log()
{
    test_section("Logging");
    fa_log_info(fa_string("---------------"));
    fa_log_info(fa_string("Log test: Do not take these seriously"));

    for (int i = 0; i < 3; ++i) {

        fa_log_info(fa_string("We have a problem"));
        // fa_log_fa_warning(fa_string("We have a problem"));
        fa_log_error(fa_string("We have a problem"));

        fa_log(NULL,
               fa_error_create_simple(
                   error,
                   fa_string("We have a problem"),
                   fa_string("Doremir.FooBar")));
        fa_thread_sleep(50);
    }

    fa_log_info(fa_string("---------------"));
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

    fa_print("exp                          ==> %s\n", fa_string_from_utf8(exp));
    fa_print("str                          ==> %s\n", fa_string_from_utf8(str));
    fa_print("matches(exp,str)             ==> %s\n", fa_fb(fa_string_matches(fa_string_from_utf8(exp), fa_string_from_utf8(str))));
}

// --------------------------------------------------------------------------------

// void test_file_stream(fa_string_t in_path, fa_string_t out_path)
// {
//     test_section("File streams");
//
//     fa_file_device_t    input, output;
//     fa_file_result_t    result;
//     // processor_t proc;
//
//     // Processor to use
//     // proc    = fa_processor_identity(type_pair(type_frame(type(fa_f32)), type_frame(type(fa_f32))));
//     proc = NULL;
//
//     // Open streams
//     input   = fa_device_file_open(in_path);
//     output  = fa_device_file_open(out_path);
//
//     // Handle possible errors
//     if (fa_check(input)) {
//         log_error((fa_error_t) input);
//         fa_warn(fa_string("Aborting test due to error"));
//         goto cleanup;
//     }
//
//     if (fa_check(output)) {
//         log_error((fa_error_t) output);
//         fa_warn(fa_string("Aborting test due to error"));
//         goto cleanup;
//     }
//
//     result = fa_device_file_run(input, proc, output);
//
//     // Handle possible error
//     if (fa_check(result)) {
//         log_error((fa_error_t) result);
//         fa_warn(fa_string("Aborting test due to error"));
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

fa_ptr_t test_audio_stream_status_changed(fa_ptr_t ct)
{
    printf("Status changed: %s!\n", fa_unstring(ct));
    return 0;
}

void test_audio_stream()
{
    test_section("Audio streams");

    fa_audio_session_t session;
    fa_audio_device_t  input = NULL, output = NULL;
    fa_audio_stream_t  stream = NULL;
    // processor_t     proc1, proc2;

    // Processor to use
    // proc1    = id(type_pair(type_frame(type(fa_f32)), type_frame(type(fa_f32))));
    // proc2    = seq(proc1, proc1);

    // Begin session
    session = fa_audio_begin_session();

    // Handle possible error
    if (fa_check(session)) {
        fa_error_log(NULL, (fa_error_t) session);
        fa_warn(fa_string("Aborting test due to error"));
        goto cleanup;
    }

    input = fa_audio_default_input(session);
    output = fa_audio_default_output(session);

    // Start stream
    stream = fa_audio_open_stream(input, NULL, NULL, output);

    // Handle possible error
    if (fa_check(stream)) {
        fa_error_log(NULL, (fa_error_t) stream);
        fa_warn(fa_string("Aborting test due to error"));
        goto cleanup;
    }

    fa_audio_add_status_callback(test_audio_stream_status_changed, fa_string("foobar"), session);
    fa_thread_sleep(3000);

cleanup:
    fa_audio_close_stream(stream);
    fa_audio_end_session(session);
}



// --------------------------------------------------------------------------------

void print_midi_devices(fa_midi_session_t session)
{
    fa_print("\n", NULL);
    fa_print("    Listing midi devices: \n", NULL);
    fa_for_each(x, fa_midi_all(session)) {
        fa_print("        Device: %s\n", x);
        fa_print("            Input:  %s\n", fa_fb(fa_midi_has_input(x)));
        fa_print("            Output: %s\n", fa_fb(fa_midi_has_output(x)));
    }
    fa_print("    Default input is : %s\n", fa_midi_default_input(session));
    fa_print("    Default output is : %s\n", fa_midi_default_output(session));
    fa_print("\n", NULL);
}

fa_ptr_t to_note_on(fa_ptr_t occ)
{
    // fa_print("%s\n", occ);
    int16_t kc = fa_ti16(fa_list_head(occ));
    return fa_midi_message(0x90, 48 + kc, 120);
}

fa_ptr_t to_note_off(fa_ptr_t occ)
{
    // fa_print("%s\n", occ);
    int16_t kc = fa_ti16(fa_list_head(occ));
    return fa_midi_message(0x80, 48 + kc, 120);
}

fa_ptr_t to_control(fa_ptr_t occ)
{
    // fa_print("%s\n", occ);
    double x = fa_tf64(fa_pair_first(occ));
    return fa_midi_message(0xb0, 7, x / 1900 * 127);
}
fa_ptr_t to_control2(fa_ptr_t occ)
{
    // fa_print("%s\n", occ);
    double y = fa_tf64(fa_pair_second(occ));
    return fa_midi_message(0xb0, 1, y / 1200 * 127);
}


void test_midi_stream()
{
    test_section("Midi streams");

    fa_midi_session_t session;
    fa_midi_device_t  input, output;
    fa_midi_stream_t  in_stream, out_stream;

    // Begin session
    session = fa_midi_begin_session();

    // Handle possible error
    if (fa_check(session)) {
        fa_error_log(NULL, (fa_error_t) session);
        fa_warn(fa_string("Aborting test due to error"));
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
        fa_error_log(NULL, (fa_error_t) in_stream);
        fa_warn(fa_string("Aborting test due to error"));
        goto cleanup;
    }

    if (fa_check(out_stream)) {
        fa_error_log(NULL, (fa_error_t) out_stream);
        fa_warn(fa_string("Aborting test due to error"));
        goto cleanup;
    }

    // TODO
    // fa_midi_add_status_callback(test_audio_stream_status_changed, fa_string("foobar"), session);

    // fa_event_t notes  =
    //     merge_event(later(divisions(1,10), midi(0x90, 48, 10)),
    //     merge_event(later(divisions(2,10), midi(0x90, 50, 20)),
    //     merge_event(later(divisions(3,10), midi(0x90, 52, 30)),
    //     merge_event(later(divisions(4,10), midi(0x90, 53, 40)),
    //     merge_event(later(divisions(5,10), midi(0x90, 55, 50)),
    //     merge_event(later(divisions(6,10), midi(0x90, 57, 60)),
    //     merge_event(later(divisions(7,10), midi(0x90, 59, 70)),
    //     merge_event(later(divisions(8,10), midi(0x90, 60, 80)),
    //     never()))))))));

    // fa_event_t notes =
    //     merge_event(fa_event_map(apply1, to_note_on,  fa_system_event_key_down()),
    //     merge_event(fa_event_map(apply1, to_note_off, fa_system_event_key_up()),
    //     merge_event(fa_event_map(apply1, to_control,  fa_system_event_mouse_move()),
    //                 fa_event_map(apply1, to_control2, fa_system_event_mouse_move()))));

    // fa_event_t notes2 = fa_event_before(later(seconds(3),0), notes);

    // fa_event_t notes   = fa_event_receive((fa_sender_t) in_stream, fa_i32(0));
    // fa_event_t sender  = fa_event_send((fa_receiver_t) out_stream, fa_i32(0), notes);
    // fa_event_t sender2 = fa_system_event_write_std(notes);

    // fa_scheduler_t sched = fa_scheduler_create(fa_time_get_system_prec_clock());
    // fa_scheduler_schedule(sched, sender);
    // fa_scheduler_schedule(sched, sender2);
    // fa_scheduler_loop(sched);

    // for (int i = 0; i < 30; ++i) {
    //     fa_message_send((fa_receiver_t) out_stream, 0, midi(0x90, 48 + i * 2, 100));
    //     fa_thread_sleep(100);
    // }

cleanup:
    // fa_midi_close_stream(stream);
    fa_midi_end_session(session);
}



void test_midi_hotplug()
{
    test_section("Midi hot-plugging");

    fa_midi_session_t session;

    // Begin session
    session = fa_midi_begin_session();

    // Handle possible error
    if (fa_check(session)) {
        fa_error_log(NULL, (fa_error_t) session);
        fa_warn(fa_string("Aborting test due to error"));
        goto cleanup;
    }

    fa_midi_add_status_callback(test_audio_stream_status_changed, fa_string("hello"), session);

    // CFRunLoopRun();
    // fa_thread_sleep(20000);
cleanup:
    // fa_midi_close_stream(stream);
    fa_midi_end_session(session);
}


// fa_ptr_t print_signal(fa_ptr_t data, fa_ptr_t value)
// {
//     fa_print("   %s\n", value);
//     return value;
// }
// fa_ptr_t signal_succ(fa_ptr_t data, fa_ptr_t value)
// {
//     return fa_add(value, fa_i16(1));
// }
// #define ap fa_signal_apply
// #define ap2(f,x,y) ap(ap(f,x),y)
// #define constant(x) fa_signal_constant(x)
// void test_signal()
// {
//     test_section("Signals");
//
//     // fa_signal_t s = fa_signal_constant(fa_i16(1));
//     // fa_signal_t p = fa_signal_constant(fa_i16(10));
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
    fa_print("%s\n", fa_version());
    fa_print("%s\n", fa_version_string());
}

// --------------------------------------------------------------------------------

static const int  iterations_k = 1;
static const bool stop_k       = false;

static int  test_function_count = 0;
static void (*test_function[2000])();
#define add_test(name) test_function[test_function_count++] = test_##name

// --------------------------------------------------------------------------------

int main(int argc, char const *argv[])
{
    char *bits      = sizeof(void *) == 4 ? "32-bit" : "64-bit";
    printf("Fa %s v%s\n", bits, fa_unstring(fa_version_string()));

    printf("sizeof(fa_ptr_t) = %d\n", (unsigned int) sizeof(fa_ptr_t));
    printf("sizeof(int32_t) = %d\n", (unsigned int) sizeof(int32_t));
    printf("sizeof(int64_t) = %d\n", (unsigned int) sizeof(int64_t));
    printf("sizeof(wchar_t) = %d\n", (unsigned int) sizeof(wchar_t));

    for (int i = 0; i < iterations_k; ++i) {
        if (stop_k) {
            getchar();
        }

        fa_set_log_std();
        // fa_set_log_file(fa_string("/Users/hans/Library/Logs/FAAudio.log"));

        fa_initialize();

        add_test(alloc);
        add_test(types);
        add_test(value_references);
        add_test(generic_functions);
        add_test(string);
        add_test(show);
        add_test(compare);
        add_test(rational);
        add_test(buffer);
        add_test(buffer_zip_unzip);
        add_test(buffer_meta);
        add_test(time);
        // test_system_time();
        // test_type();
        add_test(midi_message);

        add_test(thread);
        add_test(mutex);

        add_test(atomic);
        add_test(atomic_queue);
        // add_test(atomic_stack);
        add_test(atomic_ring_buffer);

        add_test(for_each);
        add_test(list);
        add_test(set);
        add_test(map);
        // test_graph(fa_string_dappend(fa_system_directory_current(), fa_string("/test/gen.dot")));
        // test_priority_queue(10);
        // test_json(fa_string_dappend(fa_system_directory_current(), fa_string("/test/example.json")));

        add_test(log);
        add_test(error);
        add_test(system_directory);
        add_test(regex);

        for (int i = 0; i < test_function_count; ++i) {
            test_function[i]();
        }

// end:
        fa_terminate();
    }

    return 0;
}

