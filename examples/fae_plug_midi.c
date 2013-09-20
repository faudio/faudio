
#include <fa/fa.h>
#include <fa/util.h>
#include <CoreMidi/MIDIServices.h>

ptr_t status_callback(ptr_t c, ptr_t v)
{
    printf("Midi status changed!\n");
    printf("    Sources:            %d\n", (int)MIDIGetNumberOfSources());
    printf("    Destinations:       %d\n", (int)MIDIGetNumberOfDestinations());
    printf("    Devices:            %d\n", (int)MIDIGetNumberOfDevices());

    return 0;
}

ptr_t listen(ptr_t c)
{
    fa_midi_set_status_callback(
        (fa_midi_status_callback_t) status_callback,
        NULL,
        (midi_session_t) c);

    assert(false && "Does not return...");
}

int main(int argc, char const *argv[])
{
    midi_session_t session;

    fa_fa_initialize();
    session = fa_midi_begin_session();

    if (fa_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // This should fail (and does!)
    // thread_t listen_thread = fa_thread_create(listen, (ptr_t) session);
    // fa_thread_join(listen_thread);

    listen(session);

cleanup:
    fa_midi_end_session(session);
    return 0;
}