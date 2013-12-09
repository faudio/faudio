
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program prints the currently connected audio devices.

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
    // fa_print("Listing audio devices:\n", 0);
    // fa_print_ln(string(""));

    fa_for_each(x, fa_audio_all(session)) {
        fa_print("Name: %s\n", fa_audio_name(x));
        fa_print("In:   %s\n", i16(fa_audio_input_channels(x)));
        fa_print("Out:  %s\n", i16(fa_audio_output_channels(x)));
        fa_print("Host:  %s\n", fa_audio_host_name(x));
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

