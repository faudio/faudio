
#include <fae/fae.h>
#include <fae/util.h>

/*
    This program prints the currently connected MIDI devices.
    
    The output consists of device declarations separated by blank lines.

    An example device declaration:

        Name:           Built-in Microphone
        Host:           Core Audio
        Input:          2
        Output:         0     
        
    The actual output has no indentation.
 */

fae_audio_session_t print_audio_devices(fae_ptr_t _, audio_session_t session)
{
    fae_for_each(x, fae_audio_all(session)) {
        fae_print("Name:           %s\n", fae_string_to_string(fae_audio_name(x)));
        fae_print("Host:           %s\n", fae_string_to_string(fae_audio_host_name(x)));
        fae_print("Input:          %s\n", i16(fae_type_channels(fae_audio_input_type(x))));
        fae_print("Output:         %s\n", i16(fae_type_channels(fae_audio_output_type(x))));
        fae_print_ln(string(""));
    }
    return session;
}

int main (int argc, char const *argv[])
{
    fae_fae_initialize();
    fae_audio_with_session(
        print_audio_devices, NULL, 
        fae_error_log, NULL);
    fae_fae_terminate();
}

