
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program prints the default MIDI devices.

    The output has the following format:

        Input:          Network Session 1
        Output:         Network Session 1

 */

fa_midi_session_t print_midi_devices(fa_ptr_t _, midi_session_t session)
{
    fa_midi_device_t input  = fa_midi_default_input(session);
    fa_midi_device_t output = fa_midi_default_input(session);

    if (fa_check(input)) {
        fa_print("(No inputs)\n", NULL);
    } else {
        fa_print("Input:          %s\n", fa_string_to_string(
                     fa_midi_name(input)));

    }
    if (fa_check(output)) {
        fa_print("(No outputs)\n", NULL);
    } else {
        fa_print("Output:         %s\n", fa_string_to_string(
                     fa_midi_name(output)));
    }

    return session;
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    fa_midi_with_session(
        print_midi_devices, NULL,
        fa_error_log, NULL);
    fa_fa_terminate();
}
