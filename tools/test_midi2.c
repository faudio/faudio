
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */

list_t _just(ptr_t x, list_t xs)
{
    return x;
}

void helper_function()
{
    audio_session_t audio_session = fa_audio_begin_session();
    midi_session_t midi_session = fa_midi_begin_session();

    audio_device_t audio_in = fa_audio_default_input(audio_session);
    audio_device_t audio_out = fa_audio_default_output(audio_session);
    audio_stream_t audio_stream = fa_audio_open_stream(audio_in, audio_out, _just, fa_pair_to_list(fa_signal_dls()));

    midi_device_t midi_in = fa_midi_default_input(midi_session);
    midi_device_t midi_out = fa_midi_default_output(midi_session);
    midi_stream_t midi_in_stream = fa_midi_open_stream(midi_in);
    midi_stream_t midi_out_stream = fa_midi_open_stream(midi_out);

    fa_midi_set_clock(midi_in_stream, fa_audio_stream_clock(audio_stream));
    fa_midi_set_clock(midi_out_stream, fa_audio_stream_clock(audio_stream));


    fa_print_ln(string("Running!"));
    fa_thread_sleep(1200);


    fa_destroy(midi_in_stream);
    fa_destroy(midi_out_stream);
    // fa_thread_sleep(1200);
    fa_destroy(audio_stream);

    fa_print_ln(string("Done!"));

    mark_used(midi_in_stream);
    mark_used(midi_out_stream);
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    helper_function();

    fa_terminate();
}

