
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program plays a couple of notes on the standard MIDI output.
 */
void run_midi()
{
    fa_midi_session_t s = fa_midi_begin_session();
    // fa_midi_device_t i  = fa_midi_default_input(s);

    fa_midi_device_t o  = fa_midi_default_output(s);
    assert(!fa_check(o) && "No output");

    fa_midi_stream_t st = fa_midi_open_stream(o);

    if (fa_check(st)) {
        fa_error_log(st, NULL);
    }

    for (int i = 0; i < 10; ++i) {
        fa_midi_schedule_relative(
            hms(0, 0, 0),
            fa_action_send(string("midi"), fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
            st);
        fa_thread_sleep(100);
    }

    fa_destroy(st);
    fa_destroy(s);
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();

    run_midi();

    fa_fa_terminate();
}
