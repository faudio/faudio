
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program listens for incoming MIDI notes on the default MIDI input device:

        * Prints incoming notes on the standard output

        * Echoes incoming notes to the standard MIDI output device

        * Echoes incoming notes to the standard audio output device
 */

#define kEchoPrint 0
#define kEchoMIDI 1
#define kEchoDLS 2

#define kModeOfEchoing kEchoDLS

list_t just(ptr_t x, list_t xs)
{
    return x;
}

fa_ptr_t just_print(ptr_t _, ptr_t timeMessage)
{
    // fa_print_ln(fa_string_show(timeMessage));
    fa_time_t time = fa_pair_first(timeMessage);
    fa_time_t msg  = fa_pair_second(timeMessage);
    printf("%lld ", fa_time_to_milliseconds(time));
    fa_print("%s\n", msg);

    fa_destroy(fa_pair_first(timeMessage));
    fa_destroy(fa_pair_second(timeMessage));
    fa_destroy(timeMessage);
    return 0;
}

fa_ptr_t print_and_echo_midi(ptr_t x, ptr_t timeMessage)
{
    fa_midi_stream_t out_stream = x;

    // fa_print_ln(fa_string_show(timeMessage));
    fa_time_t time = fa_pair_first(timeMessage);
    fa_time_t msg  = fa_pair_second(timeMessage);
    printf("%lld ", fa_time_to_milliseconds(time));
    fa_print("%s\n", msg);

    printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
    fa_midi_schedule_relative(fa_milliseconds(0), fa_action_send(string(""), msg), out_stream);

    fa_destroy(fa_pair_first(timeMessage));
    fa_destroy(timeMessage);
    return 0;
}

fa_ptr_t print_and_echo_dls(ptr_t x, ptr_t timeMessage)
{
    fa_audio_stream_t out_stream = x;

    // fa_print_ln(fa_string_show(timeMessage));
    fa_time_t time = fa_pair_first(timeMessage);
    fa_time_t msg  = fa_pair_second(timeMessage);
    printf("%lld ", fa_time_to_milliseconds(time));
    fa_print("%s\n", msg);

    printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
    fa_audio_schedule_relative(fa_milliseconds(0), fa_action_send(string("DLS"), msg), out_stream);
    // mark_used(out_stream);

    fa_destroy(fa_pair_first(timeMessage));
    fa_destroy(timeMessage);
    return 0;

    mark_used(time);
}

ptr_t times2(ptr_t _, ptr_t x)
{
    return fa_multiply(x, fa_signal_constant(2.0));
}

void run_midi()
{
    fa_midi_session_t s     = fa_midi_begin_session();
    fa_audio_session_t as   = fa_audio_begin_session();
    assert(s  && "No MIDI session");
    assert(as && "No audio session");

    fa_midi_device_t i  = fa_midi_default_input(s);
    fa_midi_device_t o  = fa_midi_default_output(s);
    assert(i && "No MIDI input");
    assert(o && "No MIDI output");

    fa_audio_device_t ai   = fa_audio_default_input(as);
    fa_audio_device_t ao   = fa_audio_default_output(as);
    assert(ai && "No audio input");
    assert(ao && "No audio output");

    fa_midi_stream_t ist = fa_midi_open_stream(i);
    fa_midi_stream_t ost = fa_midi_open_stream(o);

#ifndef _WIN32
    fa_pair_t synth = fa_signal_dls();
#else
    fa_pair_t synth = fa_signal_synth(string("C:\\sf.sf2"));
#endif
    list_t out              = fa_pair_to_list(synth);
    fa_audio_set_parameter(string("sample-rate"), f64(48000), as);
    fa_audio_set_parameter(string("vector-size"), f64(512), as);
    fa_audio_stream_t aost  = fa_audio_open_stream(ai, ao, just, out);

    switch (kModeOfEchoing) {

    case kEchoPrint:
        fa_midi_add_message_callback(just_print, NULL, ist);
        break;

    case kEchoMIDI:
        printf("Echoing via MIDI\n");
        fa_midi_add_message_callback(print_and_echo_midi, ost, ist);
        break;

    case kEchoDLS:
        printf("Echoing via DLS\n");
        fa_midi_add_message_callback(print_and_echo_dls, aost, ist);
        break;
    }

    if (fa_check(ist)) {
        fa_error_log(ist, NULL);
    }

    while (1) {
        fa_thread_sleep(1000);
        // fa_audio_schedule_relative(hms(0,0,0), fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 64 + ((0 % 12) * 3), 90)), aost);
    }

    fa_destroy(ist);
    fa_destroy(s);
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    run_midi();

    fa_terminate();
}
