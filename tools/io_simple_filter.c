
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/io.h>
#include "common.h"

/*
    This program does ...

 */



static inline
void push(fa_ptr_t x, fa_buffer_t buffer)
{
    printf("-----------> I was pushed!!\n");
}

static inline
void pull(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    // printf("-----------> I was pulled!!\n");
}

fa_io_filter_t fa_io_create_test_filter()
{
    return fa_io_create_simple_filter(push, pull, NULL);
}

int main(int argc, char const *argv[])
{
    fa_with_faudio() {
        fa_io_filter_t filter = fa_io_create_test_filter();
        fa_io_sink_t sink = (fa_io_sink_t) filter;


        for (int i = 0; i < 3; ++i) {
            buffer_t buf = fa_buffer_create(3);
            fa_buffer_set(buf, 0, 1);
            fa_buffer_set(buf, 0, 2);
            fa_buffer_set(buf, 0, 3);
            fa_io_push(sink, buf);
        }

        // fa_io_run(
        //     fa_io_standard_in(),
        //     fa_io_coapply(fa_io_split(sink), fa_io_standard_out())
        //         );
    }
}
