
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
        vorbis_block*       block;
        vorbis_dsp_state*   dsp;
        vorbis_info*        info;
    }                       vorbis;
    struct {
        ogg_stream_state*   stream;
    }                       ogg;
};

void write_page(struct ogg_encoder* encoder, ogg_page *page, fa_io_callback_t cb, ptr_t data);

void prepare(fa_ptr_t x)
{
    struct ogg_encoder *encoder = (struct ogg_encoder*) x;
    mark_used(encoder);

    vorbis_info_init(encoder->vorbis.info);
    
    int ret;            
    // TODO
    ret=vorbis_encode_init_vbr(encoder->vorbis.info, 1, 44100, 1.0f);
    if(ret) exit(1);
    
    /* set up the analysis state and auxiliary encoding storage */
    vorbis_analysis_init(encoder->vorbis.dsp,encoder->vorbis.info);
    vorbis_block_init(encoder->vorbis.dsp, encoder->vorbis.block);

    /* set up our packet->stream encoder */
    /* pick a random serial number; that way we can more likely build
     chained streams just by concatenation */
    ogg_stream_init(encoder->ogg.stream,rand());

    ogg_packet header;
    ogg_packet header_comm;
    ogg_packet header_code;
    
    vorbis_comment   comment; /* struct that stores all the user comments */
    
    /* add a comment */
    vorbis_comment_init(&comment);
    vorbis_comment_add_tag(&comment,"ENCODER","Faudio Vorbis encoder");
    
    vorbis_analysis_headerout(encoder->vorbis.dsp,&comment,&header,&header_comm,&header_code);
    ogg_stream_packetin(encoder->ogg.stream, &header); /* automatically placed in its own
                                       page */
    ogg_stream_packetin(encoder->ogg.stream,&header_comm);
    ogg_stream_packetin(encoder->ogg.stream,&header_code);
    
    ogg_page         page;
    
    /* This ensures the actual
     * audio data will start on a new page, as per spec
     */
    while(true){
        int result=ogg_stream_flush(encoder->ogg.stream,&page);
        if(result==0)break;

        // FIXME
        // write_page(encoder, &page);
    }

    vorbis_comment_clear(&comment);    
}

buffer_t double2float(buffer_t x) {
    buffer_t y = fa_buffer_create(fa_buffer_size(x) / 2);
    double* rx = fa_buffer_unsafe_address(x);
    float* ry = fa_buffer_unsafe_address(y);
    for (size_t i = 0; i < fa_buffer_size(x) / 8; ++i) {
        ry[i] = rx[i];
    }
    return y;
}

// Convert interleaved float buffer to raw floats
void deinterleave(float** dest, buffer_t floats, size_t channels)
{
    float* raw_floats = fa_buffer_unsafe_address(floats);
    size_t samples = fa_buffer_size(floats) / 4;
    size_t frames = samples / channels;
    for (size_t c = 0; c < channels; ++c) {
        for (size_t i = 0; i < frames; ++i) {
            dest[c][i] = raw_floats[i*channels+c];
        }
    }
    
}

void push_uncompressed(fa_ptr_t x, fa_buffer_t buffer)
{
    struct ogg_encoder *encoder = (struct ogg_encoder*) x;
    warn(string("OGG encoder assumes mono 44100"));

    int samples = fa_buffer_size(buffer) / (8/**2*/);     // Samples vs frames?
    float** buf = vorbis_analysis_buffer(
        encoder->vorbis.dsp, 
        samples
        ); 
    deinterleave(buf, double2float(buffer), 1);
    vorbis_analysis_wrote(
        encoder->vorbis.dsp,
        samples
        );

    while ((vorbis_analysis_blockout(encoder->vorbis.dsp,encoder->vorbis.block) > 0)) // TODO handle errors
    {         
        int ret;
        ret = vorbis_analysis(
            encoder->vorbis.block, 
            NULL // we use bitrate encoding
            );
        if (ret) exit(1);
        ret = vorbis_bitrate_addblock(encoder->vorbis.block); 
        if (ret) exit(1);
        // Now block is free to be reused and vorbis.dsp modified to include block (get with flushpacket)
    }

    mark_used(encoder);
}

void pull_compressed(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    struct ogg_encoder *encoder = (struct ogg_encoder*) x;

    {
        ogg_packet packet;
        vorbis_bitrate_flushpacket(encoder->vorbis.dsp, &packet);
        ogg_stream_packetin(encoder->ogg.stream, &packet);
    }
    {
        ogg_page page;
        while ((ogg_stream_pageout(encoder->ogg.stream, &page) != 0))
        {
            write_page(encoder, &page, cb, data);
            if (ogg_page_eos(&page)) {
                break;
            }
        }
    }

    mark_used(encoder);
}

void write_page(struct ogg_encoder* encoder, ogg_page *page, fa_io_callback_t cb, ptr_t data)
{
    size_t headerSize = page->header_len;
    size_t bodySize = page->body_len;
    size_t bufferSize = headerSize + bodySize;
    
    char *raw = (char*) malloc(bufferSize);
    memcpy(raw, page->header, headerSize);
    memcpy(raw+headerSize, page->body, bodySize);

    // TODO cleanup of wrapped memory
    cb(data, fa_copy(fa_buffer_wrap(raw, bufferSize, NULL, NULL)));
}


fa_io_filter_t fa_io_create_ogg_encoder()
{
    struct encoder *encoder = fa_new_struct(ogg_encoder);
    // TODO free encoder
    return fa_io_create_simple_filter(push_uncompressed, pull_compressed, encoder);
}