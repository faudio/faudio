
#include <fae/fae.h>
#include <fae/util.h>

/*
    This program prints the default MIDI devices.

    The output has the following format:

        Input:          Network Session 1
        Output:         Network Session 1

 */

fae_midi_session_t print_midi_devices(fae_ptr_t _, midi_session_t session)
{
    fae_print("Input:          %s\n", fae_string_to_string(
                  fae_midi_name(
                      fae_midi_default_input(session))));

    fae_print("Output:         %s\n", fae_string_to_string(
                  fae_midi_name(
                      fae_midi_default_output(session))));

    fae_print_ln(string(""));

    return session;
}

int main(int argc, char const *argv[])
{
    fae_fae_initialize();
    fae_midi_with_session(
        print_midi_devices, NULL,
        fae_error_log, NULL);
    fae_fae_terminate();
}
