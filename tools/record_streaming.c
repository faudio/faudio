
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include <fa/io.h>

/*
    This program does ...

    SCL_SESSION=1d6581ea210d2c66bfab1fdcdec94f03465058176fbe1baca5254d9902c6cec1
    curl -v -X POST "Accept: application/scld" \
        --cookie "session_id=$SCL_SESSION" \
        --data-binary @test/test.ogg \
        alpha.scorecloud.com:8080/api/2.0/analysis/recording-to-snippet
 */

// TODO move
#define fa_sizeof_array(A) sizeof(A) / sizeof(A[0])
#define fa_option_show_all(A,S) fa_option_show(fa_sizeof_array(A),A,S)
#define fa_option_parse_all(A,AC,AV) fa_option_parse(fa_sizeof_array(A), A, AC, AV)

void make_test_file()
{

}

static bool gVorbis;
static string_t gOutput;


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
    // signal_t i1 = fa_list_head(xs);
    pair_t synth = fa_signal_dls();
    // pair_t synth = pair(
        // fa_signal_sin(fa_signal_line(1.0/7.0)),
        // fa_signal_cos(fa_signal_line(1.0/7.0))
        // );

    fa_unpair(synth, synth1, synth2) {
        return list(
                   fa_multiply(constant(0), fa_add(constant(0), synth2)),
                   fa_multiply(constant(0), fa_signal_record_external(string("foo"), synth1)));
    }

    assert(false);
}

ptr_t _print(ptr_t x)
{
    fa_print_ln(x);
    return x;
}
fa_audio_stream_t _stream(fa_ptr_t x, fa_audio_stream_t s)
{
    fa_atomic_ring_buffer_t rbuffer = (fa_atomic_ring_buffer_t) x;
    mark_used(rbuffer);

    // TODO send
    // fa_thread_sleep(1000);

#define kRecOffset 2000

    // 10 seconds, 5 notes starting at 1 seconds (1 second between)
    fa_audio_schedule(fa_milliseconds(kRecOffset+0),      fa_action_send(string("foo"), rbuffer) , s);
    fa_audio_schedule(fa_milliseconds(kRecOffset+0),      fa_action_do(_print, string("Started recording")) , s);

    for (int i = 0; i < 5; ++i) {
        fa_audio_schedule(fa_milliseconds(kRecOffset + 1000 + (i * 1000)),  fa_action_send(string("dls"),
                          fa_midi_message_create_simple(0x90, 60 + i, 127)) , s);
    }

    fa_audio_schedule(fa_milliseconds(10000+kRecOffset),  fa_action_send(string("foo"), NULL) , s);
    fa_audio_schedule(fa_milliseconds(10000+kRecOffset),  fa_action_do(_print, string("Finished recording")) , s);

    
    printf("Started listening\n");

    // fa_thread_sleep(10500); // DEBUG Wait until rec done to remove ring buffer contention

    fa_io_run(
        fa_io_apply(
            fa_io_from_ring_buffer(rbuffer),
            
            (!gVorbis ? fa_io_identity() : fa_io_create_ogg_encoder())
        ),
        // fa_io_coapply(
            // fa_io_identity(),
            // fa_io_create_ogg_encoder(),

            fa_io_write_file(gOutput)
        // )
        );
    // fa_thread_sleep(2000);
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

fa_option_t options[] = {
    // { "v", "ogg-vorbis",    "Use ogg/vorbis compression",               fa_option_bool },
    { "o", "output-file",   "Output file (i.e. test.raw, test.ogg)",    fa_option_string },
};


int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    fa_unpair(fa_option_parse_all(options, argc, (char **) argv), opts, args) {
        mark_used(args);
        gVorbis = fa_map_get(string("ogg-vorbis"), opts)  ? fa_to_bool(fa_map_get(string("ogg-vorbis"), opts))  : false;
        gOutput = fa_map_get(string("output-file"), opts) ? fa_map_get(string("output-file"), opts) : string("test.raw");
        // printf("freq=%d, rate=%d, duration=%d\n", freq, rate, duration);

        printf("Vorbis=%d, Output=%s\n", gVorbis, unstring(gOutput));
        fa_atomic_ring_buffer_t rbuffer = atomic_ring_buffer(44100 * 8 * 30);
        mark_used(rbuffer);

        fa_audio_with_session(_session, rbuffer, fa_log, NULL);
    }
    fa_terminate();
}
