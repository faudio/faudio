
#include <fa/fa.h>
#include <fa/util.h>

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

void helper_function()
{
    fa_print_ln(string("This is fa_template!"));
}

int main(int argc, char const *argv[])
{
    fa_initialize();

    helper_function();

    fa_terminate();
}
