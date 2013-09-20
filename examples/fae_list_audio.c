
#include <fa/fa.h>
#include <fa/util.h>

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

fa_audio_session_t print_audio_devices(fa_ptr_t _, audio_session_t session)
{
    fa_for_each(x, fa_audio_all(session)) {
        fa_print("Name:           %s\n", fa_string_to_string(fa_audio_name(x)));
        fa_print("Host:           %s\n", fa_string_to_string(fa_audio_host_name(x)));
        fa_print("Input:          %s\n", i16(fa_type_channels(fa_audio_input_type(x))));
        fa_print("Output:         %s\n", i16(fa_type_channels(fa_audio_output_type(x))));
        fa_print_ln(string(""));
    }
    return session;
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    fa_audio_with_session(
        print_audio_devices, NULL,
        fa_error_log, NULL);
    fa_fa_terminate();
}

