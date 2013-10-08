
#include <fa/fa.h>
#include <fa/util.h>

void run_midi()
{
    fa_midi_session_t s = fa_midi_begin_session();
    // fa_midi_device_t i  = fa_midi_default_input(s);

    fa_midi_device_t o  = fa_midi_default_output(s);
    assert(o && "No output");

    fa_midi_stream_t st = fa_midi_open_stream(o);

    if (fa_check(st)) {
        fa_error_log(st, NULL);
    }

    for (int i = 0; true; ++i) {
        fa_midi_schedule(
            hms(0, 0, 0),
            fa_action_send(string("midi"), fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
            st);
        fa_thread_sleep(100 * 1);
    }

    fa_destroy(st);
    fa_destroy(s);
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();

    run_midi();

    fa_fa_terminate();
}
