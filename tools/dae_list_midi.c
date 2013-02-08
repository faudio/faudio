
#include <doremir/audio_engine.h>
#include <doremir/util.h>

void print_midi_devices(midi_session_t session)
{
    doremir_print("Listing midi devices: \n", NULL);
    doremir_for_each(x, doremir_device_midi_all(session)) {
        doremir_print("    Device: %s\n", x);
        doremir_print("        Input:  %s\n", b(doremir_device_midi_has_input(x)));
        doremir_print("        Output: %s\n", b(doremir_device_midi_has_output(x)));
    }
    doremir_print("Default input is : %s\n", doremir_device_midi_default_input(session));
    doremir_print("Default output is : %s\n", doremir_device_midi_default_output(session));
    doremir_print("\n", NULL);
}

int main (int argc, char const *argv[])
{
    midi_session_t session;
    
    doremir_audio_engine_initialize();
    
    session = doremir_device_midi_begin_session();

    if (doremir_check(session)) {
        log_error((error_t) session);
        goto error;
    }

    print_midi_devices(session);

    doremir_device_midi_end_session(session);
    doremir_audio_engine_terminate();
    return 0;
error:
    doremir_device_midi_end_session(session);
    doremir_audio_engine_terminate();
    return -1;
}
