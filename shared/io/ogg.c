
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
    long channels;
    long sample_rate;
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

// #define kSR       44100
// #define kChannels 1

#define byte_t uint8_t

// #define ogg_printf printf
#define ogg_printf(fmt, ...) // do nothing

void write_page(struct ogg_encoder *encoder, ogg_page *page, fa_io_callback_t cb, fa_ptr_t data);

void prepare(fa_ptr_t x, long sample_rate, long channels)
{
    struct ogg_encoder *encoder = (struct ogg_encoder *) x;
    
    encoder->sample_rate = sample_rate;
    encoder->channels    = channels;

    vorbis_info_init(&encoder->vorbis.info);

    int ret;
    // TODO
    ret = vorbis_encode_init_vbr(&encoder->vorbis.info, encoder->channels, encoder->sample_rate, 1.0f);
    if (ret) {
        fa_fail(fa_string_dappend(fa_string("Vorbis error: "), fa_string_dshow(fa_i32(ret))));
        printf("sample_rate: %ld, channels: %ld\n", sample_rate, channels);
        assert(false && "Vorbis error");
    }

    /* set up the analysis state and auxiliary encoding storage */
    vorbis_analysis_init(&encoder->vorbis.dsp, &encoder->vorbis.info);
    vorbis_block_init(&encoder->vorbis.dsp, &encoder->vorbis.block);

    /* set up our packet->stream encoder */
    /* pick a random serial number; that way we can more likely build
     chained streams just by concatenation */
    ogg_stream_init(&encoder->ogg.stream, rand());

    encoder->header_sent = false;

}

fa_buffer_t double2float(fa_buffer_t x)
{
    assert(fa_buffer_size(x) / 2);

    fa_buffer_t y = fa_buffer_create(fa_buffer_size(x) / 2);
    double *rx = fa_buffer_unsafe_address(x);
    float *ry = fa_buffer_unsafe_address(y);

    for (size_t i = 0; i < fa_buffer_size(x) / 8; ++i) {
        ry[i] = rx[i];
    }

    ogg_printf("Converting to floats: in_size=%zu, out_size=%zu\n", fa_buffer_size(x), fa_buffer_size(y));
    return y;
}

fa_buffer_t ddouble2float(fa_buffer_t x)
{
    fa_buffer_t y = double2float(x);
    fa_destroy(x);
    return y;
}

// Convert interleaved float buffer to raw floats
static inline
void fill_vorbis_buffers(float **dest, fa_buffer_t floats, size_t channels)
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
void dfill_vorbis_buffers(float **dest, fa_buffer_t floats, size_t channels)
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
        size_t frames = fa_buffer_size(buffer) / (sizeof(double) * encoder->channels);

        // This is not really documented but apparently libvorbis drops samples
        // if the buffer size is bigger than 1024
        assert(frames <= 1024 && "Vorbis analysis requre buffer size <= 1024");

        float **vorbis_buffers = vorbis_analysis_buffer(
                                     &encoder->vorbis.dsp,
                                     frames
                                 );
        // Use double2float without dealloc
        dfill_vorbis_buffers(vorbis_buffers, double2float(buffer), encoder->channels);
        vorbis_analysis_wrote(
            &encoder->vorbis.dsp,
            frames
        );

        ogg_printf("Vorbis analysis: frames=%d\n", frames);
    } else {
        vorbis_analysis_wrote(&encoder->vorbis.dsp, 0);
    }
}

void pull_compressed(fa_ptr_t x, fa_io_callback_t cb, fa_ptr_t data)
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
        ogg_stream_packetin(&encoder->ogg.stream, &header); /* automatically placed in its own page */
        ogg_stream_packetin(&encoder->ogg.stream, &header_comm);
        ogg_stream_packetin(&encoder->ogg.stream, &header_code);
        vorbis_comment_clear(&comment);

        /* This ensures the actual
         * audio data will start on a new page, as per spec
         */
        while (true) {
            ogg_page page;
            fa_mark_used(page);
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

    fa_mark_used(encoder);
}


void write_page(struct ogg_encoder *encoder, ogg_page *page, fa_io_callback_t cb, fa_ptr_t data)
{
    size_t headerSize = page->header_len;
    size_t bodySize = page->body_len;
    size_t bufferSize = headerSize + bodySize;

    byte_t *raw = (byte_t *) fa_malloc(bufferSize);
    memcpy(raw, page->header, headerSize);
    memcpy(raw + headerSize, page->body, bodySize);

    ogg_printf("                    <<< Ogg sending %zu bytes\n", bufferSize);
    fa_buffer_t buf = fa_buffer_dwrap(raw, bufferSize);
    cb(data, buf);
    fa_destroy(buf);
}

fa_ptr_t _destroy_ogg_encoder(fa_ptr_t x) {
    fa_slog_info("Cleaning up ogg encoder");
    struct ogg_encoder *encoder = (struct ogg_encoder *) x;
    ogg_stream_clear(&encoder->ogg.stream);
    vorbis_block_clear(&encoder->vorbis.block);
    vorbis_dsp_clear(&encoder->vorbis.dsp);
    vorbis_info_clear(&encoder->vorbis.info);
    fa_slog_info("Done cleaning up ogg encoder");
    return NULL;
}

fa_io_filter_t fa_io_create_ogg_encoder(long sample_rate, long channels)
{
    if (channels < 1 || channels > 2) {
        fa_fail(fa_string_format_integral("Bad number of channels in fa_io_create_ogg_encoder: %ld", channels));
        assert(false);
    }
    if (sample_rate < 11025 || sample_rate > 192400) {
        fa_fail(fa_string_format_integral("Bad sample_rate in fa_io_create_ogg_encoder: %ld", sample_rate));
        assert(false);
    }
    struct encoder *encoder = fa_new_struct(ogg_encoder);
    prepare(encoder, sample_rate, channels);
    return fa_io_create_simple_filter_with_destructor(push_uncompressed, pull_compressed, encoder, _destroy_ogg_encoder);
}
