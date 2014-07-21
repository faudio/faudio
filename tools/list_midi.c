
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program prints the currently connected MIDI devices.

    The output consists of device declarations separated by blank lines.

    An example device declaration:

        Name:           Network Session 1
        Host:           CoreMIDI
        Input:          true
        Output:         false

 */
void print_device(fa_midi_device_t x)
{
    fa_print("Name: %s\n", fa_string_to_string(fa_midi_name(x)));
    fa_print("Host: %s\n", fa_string_to_string(fa_midi_host_name(x)));
    fa_print("In:   %s\n", fa_fb(fa_midi_has_input(x)));
    fa_print("Out:  %s\n", fa_fb(fa_midi_has_output(x)));
    fa_print_ln(fa_string(""));
}

fa_midi_session_t print_midi_devices(fa_ptr_t _, fa_midi_session_t session)
{
    fa_for_each(x, fa_midi_all(session)) {
        if (!fa_check(x)) {
            print_device(x);
        }
    }

    if (!fa_check(fa_midi_default_input(session))) {
        fa_print("Default input: %s\n", fa_string_to_string(fa_midi_name(fa_midi_default_input(session))));
    } else {
        fa_print("No default input\n", NULL);
    }

    if (!fa_check(fa_midi_default_output(session))) {
        fa_print("Default output: %s\n", fa_string_to_string(fa_midi_name(fa_midi_default_output(session))));
    } else {
        fa_print("No default output\n", NULL);
    }

    return session;
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();

    fa_with_faudio() {
        fa_midi_with_session(
            print_midi_devices, NULL,
            fa_error_log, NULL);
    }
}
