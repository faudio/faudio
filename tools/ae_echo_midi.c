
#include <doremir/audio_engine.h>
#include <doremir/util.h>

void echo()
{
    midi_session_t  session;
    midi_device_t   input, output;
    midi_stream_t   in_stream, out_stream;
    event_t         send, recv;
    scheduler_t     sched;

    session = doremir_device_midi_begin_session();

    if (doremir_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    input = doremir_list_index(2, doremir_device_midi_all(session));
    output = doremir_list_index(6, doremir_device_midi_all(session));

    in_stream  = doremir_device_midi_open_stream(input);
    out_stream = doremir_device_midi_open_stream(output);

    if (doremir_check(in_stream)) {
        log_error((error_t) in_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    if (doremir_check(out_stream)) {
        log_error((error_t) out_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    recv = doremir_event_receive((sender_t) in_stream, i32(0));
    send = doremir_event_send((receiver_t) out_stream, i32(0), recv);

    sched = doremir_scheduler_create(doremir_time_get_system_prec_clock());
    doremir_scheduler_schedule(sched, send);

    printf("Input:  %s\n", unstring(doremir_device_midi_name(input)));
    printf("Output: %s\n", unstring(doremir_device_midi_name(output)));
    printf("Echoing midi...\n");
    doremir_scheduler_loop(sched);

cleanup:
    doremir_device_midi_end_session(session);
}



int main (int argc, char const *argv[])
{
    doremir_audio_engine_initialize();
    echo();
    doremir_audio_engine_terminate();
    return 0;
}



