
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
typedef fa_atomic_ring_buffer_t ring_buffer_t;

size_t fa_atomic_ring_buffer_read_many(uint8_t *dst,
                                       ring_buffer_t src,
                                       size_t count);
size_t fa_atomic_ring_buffer_write_many(ring_buffer_t dst,
                                        uint8_t *src,
                                        size_t count);


#define create_         fa_atomic_ring_buffer_create
#define read_           fa_atomic_ring_buffer_read
#define write_          fa_atomic_ring_buffer_write
#define readd_          fa_atomic_ring_buffer_read_double
#define writed_         fa_atomic_ring_buffer_write_double
#define read_many_      fa_atomic_ring_buffer_read_many
#define write_many_     fa_atomic_ring_buffer_write_many

void test_basic()
{
    ring_buffer_t b = create_(5 * sizeof(double));
    uint8_t foo[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    uint8_t *fp = foo;

    /*
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


        mark_used(fp);   */

    mark_used(fp);
    mark_used(b);
    /*
        write_many_(b, fp, 5);
        printf("%d\n", read_(b));
        printf("%d\n", read_(b));
        printf("%d\n", read_(b));
        printf("%d\n", read_(b));
        printf("%d\n", read_(b));
    */

    writed_(b, 1);
    writed_(b, 2);
    writed_(b, 3);
    writed_(b, 4);
    writed_(b, 5);

    printf("%f\n", readd_(b));
    printf("%f\n", readd_(b));
    printf("%f\n", readd_(b));
    printf("%f\n", readd_(b));
    printf("%f\n", readd_(b));

}

void test_speed()
{
    int N = 44100 * 10;
    ring_buffer_t b = create_(N * sizeof(double));

    for (int i = 0; i < N; ++i) {
        writed_(b, 1);
    }

    double x = 0;

    for (int i = 0; i < N; ++i) {
        x += readd_(b);
    }

    printf("Sum: %f!\n", x);
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    test_speed();
    fa_fa_terminate();
}
