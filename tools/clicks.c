
#include <fa/fa.h>
#include <fa/util.h>
#include "../shared/signal.h"

/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */


#define RT 1
#define kThisPlugOffset 10 // TODO

static bool should_click = false;


list_t just(ptr_t x, list_t xs)
{
    return x;
}

ptr_t before_(ptr_t x, fa_signal_state_t *state) { return x; }
ptr_t after_(ptr_t x, fa_signal_state_t *state) { return x; }
ptr_t render_(ptr_t x, fa_signal_state_t *state)
{ 
    if (!kVectorMode) {
        state->inputs[(kThisPlugOffset + 0)*kMaxVectorSize] = should_click;
        state->inputs[(kThisPlugOffset + 1)*kMaxVectorSize] = should_click;
        should_click = false;
    } else {
        for (int i = 0; i < kMaxVectorSize; ++i) {
            state->inputs[(kThisPlugOffset + 0)*kMaxVectorSize + i] = should_click;
            state->inputs[(kThisPlugOffset + 1)*kMaxVectorSize + i] = should_click;
            should_click = false;
        }
    }
    return x;
}

ptr_t receive_(ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    printf("Hello!\n");
    should_click = true;
    return x; 
}

pair_t fa_signal_clicks()
{
    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->data    = NULL;

    return pair(fa_signal_custom(proc, fa_signal_input(kThisPlugOffset+0)), fa_signal_input(kThisPlugOffset+1));
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

        for (int i = 0; true; ++i) {

            // fa_clock_t cl = fa_clock_standard();
            // fa_clock_t cl = fa_audio_stream_clock(st);
            // mark_used(cl);

            // printf("Scheduling msec: %lld \n", fa_clock_milliseconds(cl));
            // printf("Scheduling time: %s \n", unstring(fa_string_show(fa_clock_time(cl))));

            fa_action_t chord = fa_action_many(list(
                                                   pair(
                                                       fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 64 + ((i % 12) * 3), 90)),
                                                       hms(0, 0, 0)
                                                   ),
                                                   pair(
                                                       fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
                                                       hms(0, 0, 0)
                                                   )
                                               ));
            // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
            fa_audio_schedule_relative(
                hms(0, 0, 0),
                chord,
                st);
            fa_thread_sleep(150);
        }

        fa_destroy(st);
        fa_destroy(s);
    } else {
        fa_signal_run_file(44100 * 60, list(
                               pair(
                                   hms(0, 0, 0),
                                   fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((0 % 12) * 3), 90))
                               ),
                               pair(
                                   hms(0, 0, 1),
                                   fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((1 % 12) * 3), 90))
                               ),
                               pair(
                                   hms(0, 0, 2),
                                   fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60 + ((2 % 12) * 3), 90))
                               )

                           ),
                           fa_pair_first(fa_signal_clicks()),
                           string("test.wav"));
    }

}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();

    run_clicks();

    fa_fa_terminate();
}
