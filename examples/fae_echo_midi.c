
#include <fae/fae.h>
#include <fae/util.h>

midi_device_t find_device(midi_session_t session, string_t pattern) {
    fae_for_each(device, fae_midi_all(session)) {
        if (fae_string_matches(pattern,
                                   fae_midi_name(device)))
            return device;
    }
    return NULL;
}

midi_device_t find_input(midi_session_t session, string_t pattern) {
    midi_device_t device = find_device(session, pattern);
    return device ? device : fae_midi_default_input(session);
}

midi_device_t find_output(midi_session_t session, string_t pattern) {
    midi_device_t device = find_device(session, pattern);
    return device ? device : fae_midi_default_output(session);
}

void echo()
{
    midi_session_t  session;
    midi_device_t   input, output;
    midi_stream_t   in_stream, out_stream;
    // event_t         send, recv;
    // scheduler_t     sched;

    session = fae_midi_begin_session();

    if (fae_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    input       = find_input(session, string(""));
    output      = find_output(session, string(""));
    in_stream   = fae_midi_open_stream(input);
    out_stream  = fae_midi_open_stream(output);

    if (fae_check(in_stream)) {
        log_error((error_t) in_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    if (fae_check(out_stream)) {
        log_error((error_t) out_stream);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // recv = fae_event_receive((sender_t) in_stream, i32(0));
    // send = fae_event_send((receiver_t) out_stream, i32(0), recv);

    // sched = fae_scheduler_create(fae_time_get_system_prec_clock());
    // fae_scheduler_schedule(sched, send);

    printf("Input:  %s\n", unstring(fae_midi_name(input)));
    printf("Output: %s\n", unstring(fae_midi_name(output)));
    printf("Echoing midi...\n");
    // fae_scheduler_loop(sched);

cleanup:
    fae_midi_end_session(session);
}

int main (int argc, char const *argv[])
{
    fae_fae_initialize();
    echo();
    fae_fae_terminate();
    return 0;
}

