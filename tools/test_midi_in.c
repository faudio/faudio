
#include <fa/fa.h>
#include <fa/util.h>

fa_ptr_t received(ptr_t x, ptr_t timeMessage)
{
    fa_print_ln(fa_string_show(timeMessage));
    return 0;
}

void run_midi()
{
    fa_midi_session_t s = fa_midi_begin_session();
    fa_midi_device_t i  = fa_midi_default_input(s);
    assert(i && "No input");

    fa_midi_stream_t st = fa_midi_open_stream(i);
    fa_midi_add_message_callback(received, NULL, st);
    
    if (fa_check(st)) {
        fa_error_log(st, NULL);
    }

    while(1) {
        fa_thread_sleep(1000);
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
