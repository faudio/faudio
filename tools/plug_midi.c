
#include <fa/fa.h>
#include <fa/util.h>

/** Called whenever the MIDI setup changed.
 */
ptr_t status_callback(ptr_t session)
{
    printf("Midi status changed!\n");
    // printf("    Sources:            %d\n", (int)MIDIGetNumberOfSources());
    // printf("    Destinations:       %d\n", (int)MIDIGetNumberOfDestinations());
    // printf("    Devices:            %d\n", (int)MIDIGetNumberOfDevices());

    return 0;
}

/** Called whenever a new session is started.
 */
fa_midi_session_t print_midi_devices(fa_ptr_t _, fa_midi_session_t session)
{            
    fa_print_ln(fa_string_show(fa_thread_current()));    
    fa_midi_add_status_callback(status_callback, session, session);

    while (1) {
        // TODO escape when setup changed
        fa_thread_sleep(1000); 
    }
    return session;
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();

    /** While a session ends, start a new one.
     */
    while (1) {
        fa_midi_with_session(
            print_midi_devices, NULL,
            fa_error_log, NULL);
    }
    fa_fa_terminate();
}