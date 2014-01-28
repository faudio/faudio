
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/io.h>

#include <curl/curl.h>

/*
    This program does ...

    SCL_SESSION=1d6581ea210d2c66bfab1fdcdec94f03465058176fbe1baca5254d9902c6cec1
    curl -v -X POST "Accept: application/scld" \
        --cookie "session_id=$SCL_SESSION" \
        --data-binary @test/test.ogg \
        alpha.scorecloud.com:8080/api/2.0/analysis/recording-to-snippet

    VorbisInfo
        Just parameters to VorbisDSP

    VorbisBlock
        Used to perform actual analysis (side effect that alters DSP)

        vorbis_analysis
        vorbis_bitrate_addblock

    VorbisDSP
        vorbis_analysis_buffer              Get buffer to write data (array of float arrays, i.e. non-interleaved)
        vorbis_analysis_wrote               Handle data written into buffer (if any).

        vorbis_analysis_headerout           Get the 3 header packets.
        vorbis_analysis_blockout            Get an analysis block (see above, not necessarily packets)
        vorbis_bitrate_flushpacket          Get packet from analysis.
    OggStream
        Receives OGG packets, produces pages
            ogg_stream_packetin             Put in a packet.
            ogg_stream_pageout              Get out a page.
            ogg_page_eos                    Check that this page is last (end of stream).




    ----------------------------------------

    Vorbis:
        Init info
        vorbis_encode_init_vbr ???

    Init ogg()
        OGG: Init stream and write header (comm + code)
        OGG: Flush so we start at a fresh page
    Term ogg()
        ...

    Write ogg()
        For each packet
            OGG: Fill packet
            OGG: Write packet to stream state
            OGG: For each new page
                OGG: Write out the page
                OGG: Copy page header and data to raw buffer
                OGG: Write buffer to "output"

 */

void make_test_file()
{
    
}

// void convert_ogg_file()
// {
//     fa_io_run(
//         fa_io_apply(
//             fa_io_read_file(string("test/test.rawMono")),
//             // fa_io_create_ogg_encoder()
//             fa_io_identity()
//             ), 
//         
//         fa_io_coapply(
//             fa_io_identity(),
//             // fa_io_create_ogg_encoder(),
//             fa_io_write_file(string("test.raw"))
//             )
//         // fa_io_standard_out()
//         
//         );
// }
//           

list_t _signal(ptr_t x, list_t xs)
{                                                                     
    signal_t i1 = fa_list_head(xs);
    // signal_t i1 = fa_multiply(
    //     fa_multiply(
    //         fa_signal_sin(fa_signal_line(2)), 
    //         fa_signal_sin(fa_signal_line(440)) 
    //         ),
    //     constant(0.1));

    return list(constant(0), fa_multiply(constant(0), fa_signal_record_external(string("foo"), i1)));
}


fa_audio_stream_t _stream(fa_ptr_t x, fa_audio_stream_t s)
{
    fa_atomic_ring_buffer_t rbuffer = (fa_atomic_ring_buffer_t) x;
    mark_used(rbuffer);

    // TODO send
    fa_thread_sleep(1000);
    fa_audio_schedule_relative(fa_milliseconds(500),  fa_action_send(string("foo"), rbuffer) ,s);
    
    fa_io_run(fa_io_apply(fa_io_from_ring_buffer(rbuffer), 
        // fa_io_identity()
        fa_io_create_ogg_encoder()
        ),
        fa_io_write_file(string("test.ogg")));
    
    return s;
}

fa_audio_session_t _session(fa_ptr_t x, fa_audio_session_t s)
{
    fa_audio_with_stream(fa_audio_default_input(s), fa_audio_default_output(s),
        _signal, x,
        _stream, x,
        fa_log, NULL);
    return s;
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    // convert_ogg_file();
    fa_atomic_ring_buffer_t rbuffer = atomic_ring_buffer(44100*10);

    fa_audio_with_session(_session, rbuffer, fa_log, NULL);

    fa_terminate();
}
