
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/io.h>
#include <fa/util.h>

#include <ogg/ogg.h>
#include <vorbis/vorbisenc.h>
#include <vorbis/codec.h>

struct ogg_encoder {
    struct {
        vorbis_block        block;
        vorbis_dsp_state    dsp;
        vorbis_info         info;
    }                       vorbis;
    struct {
        ogg_stream_state    stream;
    }                       ogg;
    bool                    header_sent;
};

#define kSR       44100
#define kChannels 1

// #define ogg_printf printf
#define ogg_printf(fmt, ...) // do nothing

void write_page(struct ogg_encoder *encoder, ogg_page *page, fa_io_callback_t cb, ptr_t data);

void prepare(fa_ptr_t x)
{
    struct ogg_encoder *encoder = (struct ogg_encoder *) x;
    mark_used(encoder);

    vorbis_info_init(&encoder->vorbis.info);

    int ret;
    // TODO
    ret = vorbis_encode_init_vbr(&encoder->vorbis.info, kChannels, kSR, 1.0f);
    assert((ret == 0) && "Unknown error");

    /* set up the analysis state and auxiliary encoding storage */
    vorbis_analysis_init(&encoder->vorbis.dsp, &encoder->vorbis.info);
    vorbis_block_init(&encoder->vorbis.dsp, &encoder->vorbis.block);

    /* set up our packet->stream encoder */
    /* pick a random serial number; that way we can more likely build
     chained streams just by concatenation */
    ogg_stream_init(&encoder->ogg.stream, rand());

    encoder->header_sent = false;

}

buffer_t double2float(buffer_t x)
{
    assert(fa_buffer_size(x) / 2);

    buffer_t y = fa_buffer_create(fa_buffer_size(x) / 2);
    double *rx = fa_buffer_unsafe_address(x);
    float *ry = fa_buffer_unsafe_address(y);

    for (size_t i = 0; i < fa_buffer_size(x) / 8; ++i) {
        ry[i] = rx[i];
    }

    ogg_printf("Converting to floats: in_size=%zu, out_size=%zu\n", fa_buffer_size(x), fa_buffer_size(y));
    return y;
}

buffer_t ddouble2float(buffer_t x)
{
    buffer_t y = double2float(x);
    fa_destroy(x);
    return y;
}

// Convert interleaved float buffer to raw floats
static inline
void fill_vorbis_buffers(float **dest, buffer_t floats, size_t channels)
{
    float *raw_floats = fa_buffer_unsafe_address(floats);
    size_t samples = fa_buffer_size(floats) / 4;
    size_t frames = samples / channels;

    ogg_printf("Filling vorbis buffers: samples=%zu, frames=%zu, channels=%zu\n", samples, frames, channels);

    for (size_t c = 0; c < channels; ++c) {
        for (size_t i = 0; i < frames; ++i) {
            dest[c][i] = raw_floats[i * channels + c];
        }
    }

}

static inline
void dfill_vorbis_buffers(float **dest, buffer_t floats, size_t channels)
{
    fill_vorbis_buffers(dest, floats, channels);
    fa_destroy(floats);
}

void push_uncompressed(fa_ptr_t x, fa_buffer_t buffer)
{
    struct ogg_encoder *encoder = (struct ogg_encoder *) x;

    if (buffer) {
        ogg_printf(">>> Ogg received %zu bytes\n", fa_buffer_size(buffer));
    }

    // warn(string("OGG encoder assumes mono 44100"));

    if (buffer) {
        int samples = fa_buffer_size(buffer) / (8/**2*/);     // Samples vs frames?

        // This is not really documented but apparently libvorbis drops samples
        // if the buffer size is bigger than 1024
        assert(samples <= 1024 && "Vorbis analysis requre buffer size <= 1024");

        float **vorbis_buffers = vorbis_analysis_buffer(
                                     &encoder->vorbis.dsp,
                                     samples
                                 );
        // Use double2float without dealloc
        dfill_vorbis_buffers(vorbis_buffers, double2float(buffer), kChannels);
        vorbis_analysis_wrote(
            &encoder->vorbis.dsp,
            samples
        );

        ogg_printf("Vorbis analysis: samples=%d\n", samples);
    } else {
        vorbis_analysis_wrote(&encoder->vorbis.dsp, 0);
    }

    mark_used(encoder);
}

void pull_compressed(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    struct ogg_encoder *encoder = (struct ogg_encoder *) x;

    if (!encoder->header_sent) {
        encoder->header_sent = true;

        ogg_packet header;
        ogg_packet header_comm;
        ogg_packet header_code;

        vorbis_comment   comment; /* struct that stores all the user comments */

        /* add a comment */
        vorbis_comment_init(&comment);
        vorbis_comment_add_tag(&comment, "ENCODER", "Faudio Vorbis encoder");

        vorbis_analysis_headerout(&encoder->vorbis.dsp, &comment, &header, &header_comm, &header_code);
        ogg_stream_packetin(&encoder->ogg.stream, &header); /* automatically placed in its own
                                           page */
        ogg_stream_packetin(&encoder->ogg.stream, &header_comm);
        ogg_stream_packetin(&encoder->ogg.stream, &header_code);
        vorbis_comment_clear(&comment);

        /* This ensures the actual
         * audio data will start on a new page, as per spec
         */
        while (true) {
            ogg_page page;
            mark_used(page);
            int result = ogg_stream_flush(&encoder->ogg.stream, &page);

            if (result == 0) {
                break;
            }

            write_page(encoder, &page, cb, data);
        }
    }

    while (vorbis_analysis_blockout(&encoder->vorbis.dsp, &encoder->vorbis.block) == 1) {

        /* analysis, assume we want to use bitrate management */
        vorbis_analysis(&encoder->vorbis.block, NULL);
        vorbis_bitrate_addblock(&encoder->vorbis.block);

        ogg_packet packet;

        while (vorbis_bitrate_flushpacket(&encoder->vorbis.dsp, &packet)) {
            ogg_stream_packetin(&encoder->ogg.stream, &packet);

            ogg_page page;

            while (true) {
                int result = ogg_stream_pageout(&encoder->ogg.stream, &page);

                if (result == 0) {
                    break;
                }

                write_page(encoder, &page, cb, data);

                if (ogg_page_eos(&page)) {
                    ogg_printf("Vorbis analysis finished\n");

                    cb(data, NULL);
                    break;
                }
            }

        };

    }

    mark_used(encoder);
}

void write_page(struct ogg_encoder *encoder, ogg_page *page, fa_io_callback_t cb, ptr_t data)
{
    size_t headerSize = page->header_len;
    size_t bodySize = page->body_len;
    size_t bufferSize = headerSize + bodySize;

    char *raw = (char *) fa_malloc(bufferSize);
    memcpy(raw, page->header, headerSize);
    memcpy(raw + headerSize, page->body, bodySize);

    // TODO cleanup of wrapped memory
    ogg_printf("                    <<< Ogg sending %zu bytes\n", bufferSize);
    cb(data, fa_copy(fa_buffer_wrap(raw, bufferSize, NULL, NULL)));
    fa_free(raw);
}


fa_io_filter_t fa_io_create_ogg_encoder()
{
    struct encoder *encoder = fa_new_struct(ogg_encoder);
    prepare(encoder);
    // TODO free encoder
    return fa_io_create_simple_filter(push_uncompressed, pull_compressed, encoder);
}