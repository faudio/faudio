
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/io.h>
#include <fa/util.h>

struct ogg_encoder {
    int x;
};

void push_uncompressed(fa_ptr_t x, fa_buffer_t buffer)
{
    struct encoder *encoder = (struct encoder*) x;
    mark_used(encoder);
}
void pull_compressed(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    struct encoder *encoder = (struct encoder*) x;
    mark_used(encoder);
}


fa_io_filter_t fa_io_create_ogg_encoder()
{
    struct encoder *encoder = fa_new_struct(ogg_encoder);
    // TODO free encoder
    return fa_io_create_simple_filter(push_uncompressed, pull_compressed, encoder);
}