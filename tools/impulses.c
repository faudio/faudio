
#include <fa/fa.h>
#include <fa/util.h>
#include "../shared/signal.h"
#include "common.h"

/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */


#define RT 0

static bool should_click = false;


fa_ptr_t before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
fa_ptr_t after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    return x;
}
fa_ptr_t render_(fa_ptr_t x, int offset, int count, fa_signal_state_t *state)
{
    if (!kVectorMode) {
        state->buffer[(offset + 0)*kMaxVectorSize] = should_click;
        state->buffer[(offset + 1)*kMaxVectorSize] = should_click;
        should_click = false;
    } else {
        for (int i = 0; i < count; ++i) {
            state->buffer[(offset + 0)*kMaxVectorSize + i] = should_click;
            state->buffer[(offset + 1)*kMaxVectorSize + i] = should_click;
            should_click = false;
        }
    }

    return x;
}

fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    printf("Click!\n");
    should_click = true;
    return x;
}

fa_list_t fa_signal_clicks()
{
    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = NULL;
    proc->destroy = NULL;
    proc->data    = NULL;

    return list(fa_signal_custom(proc, fa_signal_input_with_custom(proc, 0)), fa_signal_input_with_custom(proc, 1));
}

void run_clicks()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_list_t out           = fa_signal_clicks();

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        for (int i = 0; true; ++i) {

            // fa_clock_t cl = fa_clock_standard();
            // fa_clock_t cl = fa_audio_stream_clock(st);
            // fa_mark_used(cl);

            // printf("Scheduling msec: %lld \n", fa_clock_milliseconds(cl));
            // printf("Scheduling time: %s \n", unstring(fa_string_show(fa_clock_time(cl))));

            fa_action_t chord = fa_action_many(list(
                                                   fa_pair_create(
                                                       fa_action_send(fa_string("DLS"), fa_midi_message_create_simple(0x90, 64 + ((i % 12) * 3), 90)),
                                                       fa_hms(0, 0, 0)
                                                   ),
                                                   fa_pair_create(
                                                       fa_action_send(fa_string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
                                                       fa_hms(0, 0, 0)
                                                   )
                                               ));
            // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
            fa_audio_schedule_relative(
                fa_hms(0, 0, 0),
                chord,
                st);
            fa_thread_sleep(150);
        }

        fa_destroy(st);
        fa_destroy(s);
    } else {
        fa_signal_run_file(44100 * 10, list(
                               fa_pair_create(
                                   fa_hms(0, 0, 0),
                                   fa_action_send(fa_string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((0 % 12) * 3), 90))
                               ),
                               fa_pair_create(
                                   fa_hms(0, 0, 1),
                                   fa_action_send(fa_string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((1 % 12) * 3), 90))
                               ),
                               fa_pair_create(
                                   fa_hms(0, 0, 2),
                                   fa_action_send(fa_string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((2 % 12) * 3), 90))
                               )

                           ),
                           fa_signal_clicks(),
                           44100,
                           fa_string("test.wav"));
    }

}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {

        run_clicks();

    }
}
