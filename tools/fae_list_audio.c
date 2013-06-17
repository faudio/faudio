
#include <fae/fae.h>
#include <fae/util.h>

void print_audio_devices(audio_session_t session)
{
    fae_print("Audio devices: \n", NULL);
    fae_for_each(x, fae_device_audio_all(session)) {
        fae_print("    Device: %s\n", x);
        fae_print("        Input:  %s\n", fae_device_audio_input_type(x));
        fae_print("        Output: %s\n", fae_device_audio_output_type(x));
    }
    fae_print("Default input is : %s\n", fae_device_audio_default_input(session));
    fae_print("Default output is : %s\n", fae_device_audio_default_output(session));
    fae_print("\n", NULL);
}

int main (int argc, char const *argv[])
{    
    audio_session_t session;    
    
    fae_fae_initialize();
    session = fae_device_audio_begin_session();

    if (fae_check(session)) {
        log_error((error_t) session);
        goto error;
    }

    print_audio_devices(session);

    fae_device_audio_end_session(session);
    fae_fae_terminate();

    return 0;

error:
    fae_device_audio_end_session(session);
    fae_fae_terminate();
    return -1;
}
