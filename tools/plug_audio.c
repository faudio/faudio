
#include <fa/fa.h>
#include <fa/util.h>

static int stop;

/** Called whenever the MIDI setup changed.
 */
ptr_t status_callback(ptr_t session)
{
    printf("Audio status changed!\n");
    stop = true;
    return 0;
}

/** Called whenever a new session is started.
 */
fa_audio_session_t print_audio_devices(fa_ptr_t _, fa_audio_session_t session)
{            
    fa_audio_add_status_callback(status_callback, session, session);

    fa_thread_sleep(500); // FIXME why is this needed?
    fa_for_each(x, fa_audio_all(session)) {
        fa_print("Name: %s\n", fa_string_to_string(fa_audio_name(x)));
        fa_print("Host: %s\n", fa_string_to_string(fa_audio_host_name(x)));
        fa_print("In:   %s\n", fb(fa_audio_has_input(x)));
        fa_print("Out:  %s\n", fb(fa_audio_has_output(x)));
        fa_print_ln(string(""));
        mark_used(x);
    }
                           
    stop = false;
    while (1) {
        // printf("Stop: %d\n", stop);
        if (stop) {
            return session;
        }
        fa_thread_sleep(1000); 
    }
    return session;
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();

    /** While a session ends, start a new one.
     */
    while (1) {
        fa_audio_with_session(
            print_audio_devices, NULL,
            fa_error_log, NULL);
    }
    fa_fa_terminate();
}