
#include <fae/fae.h>
#include <fae/util.h>
#include <fae/thread.h>
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
    fae_device_midi_set_status_callback(
        (fae_device_midi_status_callback_t) status_callback, 
        NULL, 
        (midi_session_t) c);
                          
    assert(false && "Does not return...");
}

int main (int argc, char const *argv[])
{
    midi_session_t session;

    fae_fae_initialize();
    session = fae_device_midi_begin_session();

    if (fae_check(session)) {
        log_error((error_t) session);
        warn(string("Aborting test due to error"));
        goto cleanup;
    }

    // This should fail (and does!)
    // thread_t listen_thread = fae_thread_create(listen, (ptr_t) session);
    // fae_thread_join(listen_thread);

    listen(session);

cleanup:
    fae_device_midi_end_session(session);
    return 0; 
}