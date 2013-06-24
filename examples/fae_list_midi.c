
#include <fae/fae.h>
#include <fae/util.h>

/*
    This program prints the currently connected MIDI devices.
    
    The output consists of device declarations separated by blank lines.

    An example device declaration:

        Name:           Network Session 1
        Host:           CoreMIDI
        Input:          true
        Output:         false     
        
    The actual output has no indentation.
 */

fae_midi_session_t print_midi_devices(fae_ptr_t _, midi_session_t session)
{
    fae_for_each(x, fae_midi_all(session)) {
        fae_print("Name:           %s\n", fae_string_to_string(fae_midi_name(x)));
        fae_print("Host:           %s\n", fae_string_to_string(fae_midi_host_name(x)));
        fae_print("Input:          %s\n", fb(fae_midi_has_input(x)));
        fae_print("Output:         %s\n", fb(fae_midi_has_output(x)));
        fae_print_ln(string(""));
    }
    return session;
}

int main (int argc, char const *argv[])
{
    fae_fae_initialize();
    fae_midi_with_session(
        print_midi_devices, NULL, 
        fae_error_log, NULL);
    fae_fae_terminate();
}
