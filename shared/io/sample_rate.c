
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/io.h>
#include <fa/util.h>

struct sample_rate_filter {
    size_t count;
    char buffer[256];
};

// #define sample_rate_printf printf
#define sample_rate_printf(fmt, ...) // do nothing

static inline
void prepare(fa_ptr_t x)
{
    struct sample_rate_filter *filter = (struct sample_rate_filter *) x;
    mark_used(filter);
}

static inline
void push(fa_ptr_t x, fa_buffer_t buffer)
{
    struct sample_rate_filter *filter = (struct sample_rate_filter *) x;
    mark_used(filter);
}

static inline
void pull(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    struct sample_rate_filter *filter = (struct sample_rate_filter *) x;
    mark_used(filter);
}


fa_io_filter_t fa_io_create_sample_rate_filter()
{
    struct filter *filter = fa_new_struct(sample_rate_filter);
    prepare(filter);
    // sample_rateODO free filter
    return fa_io_create_simple_filter(push, pull, filter);
}


static inline convert_sr(long count, double[] inputs, double[] outputs)
{
    // TODO
    int i, j, a, b, z;

    a = 44100;
    b = 8363;

    // upsample by a
    for (i = z = 0; i < samplen; i++)
        for (j = 0; j < a; j++) {
            cbuf[z++] = sampdata[i];
        }

    // some filter goes here???

    // downsample by b
    for (j = i = 0; i < z; i += b) {
        buf[j++] = cbuf[i];
    }
}