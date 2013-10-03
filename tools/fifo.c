
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
typedef fa_atomic_ring_buffer_t ring_buffer_t;

#define create_ fa_atomic_ring_buffer_create
#define read_   fa_atomic_ring_buffer_read
#define write_  fa_atomic_ring_buffer_write

void helper_function()
{
    ring_buffer_t b = create_(5);

    write_(b, 1);
    write_(b, 2);
    write_(b, 3);
    write_(b, 4);
    write_(b, 5);

    printf("%d\n", read_(b));
    printf("%d\n", read_(b));
    printf("%d\n", read_(b));
    printf("%d\n", read_(b));
    printf("%d\n", read_(b));

    write_(b, 6);
    printf("%d\n", read_(b));
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
