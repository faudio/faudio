
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

    {                                         
        float iv = 100;     
        time_t half     = fa_milliseconds((int) (iv/2));
        time_t interv   = fa_milliseconds((int) iv);
        fa_action_t note  = fa_action_send(string("midi"), fa_midi_message_create_simple(0x99, 60, 90));
        fa_action_t notes = fa_action_repeat(interv, note);
        fa_action_t off   = fa_action_send(string("midi"), fa_midi_message_create_simple(0x99, 61, 90));
        fa_action_t offs  = fa_action_repeat(interv, off);
        
        fa_midi_schedule_relative(seconds(0), notes, st);
        fa_midi_schedule_relative(half,       offs, st);
        fa_thread_sleep(100000);
        mark_used(half);
        mark_used(notes);
        mark_used(offs);
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
