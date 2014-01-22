
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

 */
void print_device(audio_device_t x)
{
    fa_print("Name: %s\n", fa_audio_name(x));
    fa_print("In:   %s\n", i16(fa_audio_input_channels(x)));
    fa_print("Out:  %s\n", i16(fa_audio_output_channels(x)));
    fa_print("Host: %s\n", fa_audio_host_name(x));
    fa_print("Rate: %s\n", f64(fa_audio_default_sample_rate(x)));
    fa_print_ln(string(""));
}

fa_audio_session_t print_audio_devices(fa_ptr_t _, audio_session_t session)
{
    fa_for_each(x, fa_audio_all(session)) {
        if (!fa_check(x)) {
            print_device(x);
        }
    }

    if (!fa_check(fa_audio_default_input(session))) {
        fa_print("Default input: %s\n", fa_string_to_string(fa_audio_name(fa_audio_default_input(session))));
    } else {
        fa_print("No default input\n", NULL);
    }

    if (!fa_check(fa_audio_default_output(session))) {
        fa_print("Default output: %s\n", fa_string_to_string(fa_audio_name(fa_audio_default_output(session))));
    } else {
        fa_print("No default output\n", NULL);
    }

    return session;
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();
    fa_audio_with_session(
        print_audio_devices, NULL,
        fa_error_log, NULL);
    fa_terminate();
}

