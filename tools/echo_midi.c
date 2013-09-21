
#include <fa/fa.h>
#include <fa/util.h>

midi_device_t find_device(midi_session_t session, string_t pattern)
{
    fa_for_each(device, fa_midi_all(session)) {
        if (fa_string_matches(pattern,
                               fa_midi_name(device))) {
            return device;
        }
    }
    return NULL;
}

midi_device_t find_input(midi_session_t session, string_t pattern)
{
    midi_device_t device = find_device(session, pattern);
    return device ? device : fa_midi_default_input(session);
}

midi_device_t find_output(midi_session_t session, string_t pattern)
{
    midi_device_t device = find_device(session, pattern);
    return device ? device : fa_midi_default_output(session);
}

void echo()
{
    midi_session_t  session;
    midi_device_t   input, output;
    midi_stream_t   in_stream, out_stream;
    // event_t         send, recv;
    // scheduler_t     sched;

    session = fa_midi_begin_session();

    if (fa_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    input       = find_input(session, string(""));
    output      = find_output(session, string(""));
    in_stream   = fa_midi_open_stream(input);
    out_stream  = fa_midi_open_stream(output);

    if (fa_check(in_stream)) {
        log_error((error_t) in_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    if (fa_check(out_stream)) {
        log_error((error_t) out_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // recv = fa_event_receive((sender_t) in_stream, i32(0));
    // send = fa_event_send((receiver_t) out_stream, i32(0), recv);

    // sched = fa_scheduler_create(fa_time_get_system_prec_clock());
    // fa_scheduler_schedule(sched, send);

    printf("Input:  %s\n", unstring(fa_midi_name(input)));
    printf("Output: %s\n", unstring(fa_midi_name(output)));
    printf("Echoing midi...\n");
    // fa_scheduler_loop(sched);

cleanup:
    fa_midi_end_session(session);
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    echo();
    fa_fa_terminate();
    return 0;
}

