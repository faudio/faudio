
#include <fa/fa.h>
#include <fa/util.h>
#include "../shared/signal.h"

/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */


#define RT 1
#define kThisPlugOffset 32 // TODO

static bool should_click = false;


list_t just(ptr_t x, list_t _)
{
    return x;
}

ptr_t before_(ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
ptr_t after_(ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
ptr_t render_(ptr_t x, int count, fa_signal_state_t *state)
{
    if (!kVectorMode) {
        state->buffer[(kThisPlugOffset + 0)*kMaxVectorSize] = should_click;
        state->buffer[(kThisPlugOffset + 1)*kMaxVectorSize] = should_click;
        should_click = false;
    } else {
        for (int i = 0; i < count; ++i) {
            state->buffer[(kThisPlugOffset + 0)*kMaxVectorSize + i] = should_click;
            state->buffer[(kThisPlugOffset + 1)*kMaxVectorSize + i] = should_click;
            should_click = false;
        }
    }

    return x;
}

ptr_t receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    printf("Click!\n");
    should_click = true;
    return x;
}
ptr_t send_(ptr_t x, fa_signal_message_callback_t cb, ptr_t data)
{
    // TODO should not allocate here
    // The value passed is *not* destroyed, but *will* be copied
    // All create/destroy should happen in the setup phase
    cb(data, string("foo"), i32(1));
    return x;
}


pair_t fa_signal_clicks()
{
    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = send_;
    proc->data    = NULL;

    return pair(fa_signal_custom(proc, fa_signal_input(kThisPlugOffset + 0)), fa_signal_input(kThisPlugOffset + 1));
}

void run_clicks()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out           = fa_pair_to_list(fa_signal_clicks());

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        fa_thread_sleep(10000);
        fa_destroy(st);
        fa_destroy(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    run_clicks();

    fa_terminate();
}
