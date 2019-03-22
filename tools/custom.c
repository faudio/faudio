#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"
#include "../shared/signal.h"

/*
    This program defines a custom processor and runs it on the default audio input and output devices.

 */

#define PI  3.1415
#define TAU (2 * PI)

fa_ptr_t before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    printf("Before!\n");
    return x;
}
fa_ptr_t after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    printf("After!\n");
    return x;
}
fa_ptr_t render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    // printf("Render!\n");
    state->buffer[(offset + 0) * kMaxVectorSize] = 0.01 * (state->count / state->rate);
    return x;
}
fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    printf("Received %s : %s!\n", fa_unstring(n), fa_unstring(fa_string_show(msg)));
    return x;
}

#define RT 1

void helper_function()
{
    fa_signal_custom_processor_t proc;
    proc.before  = before_;
    proc.after   = after_;
    proc.render  = render_;
    proc.receive = receive_;
    proc.send    = NULL;
    proc.data    = NULL;
    proc.destroy = NULL;

    // printf("Sending custom proc %p!\n", &proc);

    fa_signal_t a = fa_multiply(fa_signal_sin(fa_signal_line(440)), fa_signal_input(32));
    fa_signal_t b = fa_signal_custom(&proc, a);
    fa_mark_used(a);
    fa_mark_used(b);

    fa_signal_t r = b;

    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_list_t out           = list(r, a);

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        fa_audio_schedule(
            fa_hms(0, 0, 0), fa_action_send(fa_string("DLS"), fa_string("hello!")), st
        );

        while (1) {
            fa_audio_schedule(
                fa_hms(0, 0, 0), fa_action_send(fa_string("DLS"), fa_string("hello!")), st
            );
            fa_thread_sleep(1000 * 1);
        }

        fa_destroy(st);
        fa_destroy(s);
    } else {
        fa_signal_run_file(44100 * 10, list(
                               fa_pair_create(fa_hms(0, 0, 0), fa_action_send(fa_string("DLS"), fa_string("hello!")))

                           ),
                           fa_list_single(r),
                           44100,
                           fa_string("test.wav"));
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        helper_function();
    }
}
