
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

static int stop;


/** Called whenever the MIDI setup changed.
 */
ptr_t status_callback(ptr_t session)
{
    printf("Midi status changed!\n");
    stop = true;
    return 0;
}

/** Called whenever a new session is started.
 */
fa_midi_session_t print_midi_devices(fa_ptr_t _, fa_midi_session_t session)
{
    fa_midi_add_status_callback(status_callback, session, session);

    // fa_thread_sleep(500); // FIXME why is this needed?

    list_t open_streams = empty();
    fa_for_each(x, fa_midi_all(session)) {
        fa_print("Name: %s\n", fa_string_to_string(fa_midi_name(x)));
        fa_print("Host: %s\n", fa_string_to_string(fa_midi_host_name(x)));
        fa_print("In:   %s\n", fb(fa_midi_has_input(x)));
        fa_print("Out:  %s\n", fb(fa_midi_has_output(x)));
        fa_print_ln(string(""));
        mark_used(x);
    }

    fa_for_each(x, fa_midi_all(session)) {
        // if (fa_midi_has_input(x)) {
        if (!fa_string_matches(string(".*GS Wavetable.*"), fa_midi_name(x))) {
            fa_push_list(fa_midi_open_stream(x), open_streams);
        }
    }

    // TODO stop all

    stop = false;

    while (1) {
        // printf("Stop: %d\n", stop);
        if (stop) {
            return session;
        }

        fa_thread_sleep(1);
    }

    return session;
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();

    fa_with_faudio() {
        /** While a session ends, start a new one.
         */
        while (1) {
            fa_midi_with_session(
                print_midi_devices, NULL,
                fa_error_log, NULL);
        }
    }
}