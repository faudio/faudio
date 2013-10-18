
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

static mutex_t gMutex;

ptr_t foo(ptr_t _) {

    while (1) {
        fa_with_lock(gMutex)
        {
            printf("Foo!\n");
        }
    }
    return NULL;
}

ptr_t bar(ptr_t _) {
    while (1) {
        fa_with_lock(gMutex)
        {
            printf("Bar!\n");
            continue;
        }
    }
    return NULL;
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();

    // test_macros();
    
    gMutex = fa_thread_create_mutex();
    fa_thread_create(foo, NULL);
    fa_thread_create(bar, NULL);
    fa_thread_sleep(100000);

    fa_fa_terminate();
}
