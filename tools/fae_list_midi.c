
#include <fae/fae.h>
#include <fae/util.h>

void print_midi_devices(midi_session_t session)
{
    fae_print("Listing midi devices: \n", NULL);
    fae_for_each(x, fae_device_midi_all(session)) {
        fae_print("    Device: %s\n", x);
        fae_print("        Input:  %s\n", fb(fae_device_midi_has_input(x)));
        fae_print("        Output: %s\n", fb(fae_device_midi_has_output(x)));
    }
    fae_print("Default input is : %s\n", fae_device_midi_default_input(session));
    fae_print("Default output is : %s\n", fae_device_midi_default_output(session));
    fae_print("\n", NULL);
}

int main (int argc, char const *argv[])
{
    midi_session_t session;
    
    fae_fae_initialize();
    
    session = fae_device_midi_begin_session();

    if (fae_check(session)) {
        log_error((error_t) session);
        goto error;
    }

    print_midi_devices(session);

    fae_device_midi_end_session(session);
    fae_fae_terminate();
    return 0;
error:
    fae_device_midi_end_session(session);
    fae_fae_terminate();
    return -1;
}
