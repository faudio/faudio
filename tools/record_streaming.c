
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
    fa_thread_sleep(1000);
    fa_audio_schedule_relative(fa_milliseconds(500),         fa_action_send(string("foo"), rbuffer) ,s);
    fa_audio_schedule_relative(fa_milliseconds(500),         fa_action_do(_print, string("Started recording")) ,s);
    fa_audio_schedule_relative(fa_milliseconds(20000 + 500),  fa_action_send(string("foo"), NULL) ,s);
    fa_audio_schedule_relative(fa_milliseconds(20000 + 500),  fa_action_do(_print, string("Finished recording")) ,s);
    // fa_thread_create(_thread, rbuffer);
    // fa_thread_sleep(7000);
    
    fa_io_run(
        fa_io_apply(
            // fa_io_read_file(string("test.rawMono")), 
            fa_io_from_ring_buffer(rbuffer), 

            // fa_io_identity()
           fa_io_create_ogg_encoder() 

        ),
        fa_io_write_file(string("test.ogg")));
    fa_thread_sleep(2000);
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
    fa_atomic_ring_buffer_t rbuffer = atomic_ring_buffer(1024*40);
    mark_used(rbuffer);

    fa_audio_with_session(_session, rbuffer, fa_log, NULL);
    // fa_io_run(
    //     fa_io_apply(
    //         fa_io_read_file(string("test/test.rawMono")), 
    //         // fa_io_from_ring_buffer(rbuffer), 
    // 
    //         // fa_io_identity()
    //        fa_io_create_ogg_encoder() 
    // 
    //     ),
    //     fa_io_write_file(string("test.ogg")));

    fa_terminate();
}
