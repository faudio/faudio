
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */

void test_macros()
{

    fa_let(i, 1) {
        printf("i = %d\n", i);
    }

    fa_let(i, 1) {
        mark_used(i);
        fa_let(i, 2) {
            printf("i = %d\n", i);
        }
    }

    printf("\n");

    {
        int j = 0;
        fa_with(i, 1, j = 2) {
            printf("i = %d\n", i);
            continue;
        }
        printf("after: j = %d\n", j);
    }
}

ptr_t foo(ptr_t mutex)
{

    while (1) {
        fa_with_lock(mutex) {
            printf("Foo!\n");
        }
    }

    return NULL;
}

ptr_t bar(ptr_t mutex)
{
    while (1) {
        fa_with_lock(mutex) {
            // break; // This deadlocks
            continue; // This does not
            printf("Bar!\n");
        }
    }

    return NULL;
}

int main(int argc, char const *argv[])
{
    fa_initialize();

    // test_macros();

    fa_with_temp(mutex, fa_thread_create_mutex()) {
        fa_thread_create(foo, mutex);
        fa_thread_create(bar, mutex);
        fa_thread_sleep(100000);
    }

    fa_terminate();
}
