
#include <fa/fa.h>
#define NO_THREAD_T
#include <fa/util.h>
#undef NO_THREAD_T
#include "common.h"

#ifdef __APPLE__
#include <ApplicationServices/ApplicationServices.h> // DEBUG
#endif

/*
    This program tests compound scheduling.

    TODO
 */


bool pred1(ptr_t _, ptr_t x)
{
#ifdef __APPLE__
    CGEventRef event = CGEventCreate(nil);
    CGPoint loc = CGEventGetLocation(event);
    inform(fa_string_format_floating("x: %f", loc.x));
    bool res = loc.x > 200;
    CFRelease(event);

    return res;
#else
    return true;
#endif
}

bool pred2(ptr_t _, ptr_t x)
{
#ifdef __APPLE__

    CGEventRef event = CGEventCreate(nil);
    CGPoint loc = CGEventGetLocation(event);
    inform(fa_string_format_floating("x: %f", loc.x));
    bool res = loc.y > 200;
    CFRelease(event);

    return res;
#else
    return true;
#endif
}

ptr_t _print(ptr_t x)
{
    printf("Hello!\n");
    return NULL;
}

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
        fa_audio_session_t as = fa_audio_begin_session();
        fa_audio_device_t ai = fa_audio_default_input(as);
        fa_audio_device_t ao = fa_audio_default_output(as);
        fa_audio_stream_t ast = fa_audio_open_stream(ai, ao, NULL, NULL);
        fa_print_ln(ast);

        mark_used(ast);

        // Use audio clock
        fa_midi_set_clock(st, fa_audio_stream_clock(ast));
    }

    {

        fa_action_t note1  = fa_action_send(string("midi"), fa_midi_message_create_simple(0x90, 60, 127));
        fa_action_t note2  = fa_action_send(string("midi"), fa_midi_message_create_simple(0x90, 65, 127));

        // fa_action_t notes1 = fa_action_many(list(
        //                                         fa_pair_create(note2, fa_milliseconds(100 + 200 + 100)),
        //                                         fa_pair_create(note2, fa_milliseconds(100 + 100))
        //                                     ));
        // fa_action_t notes2 = fa_action_many(list(
        //                                         fa_pair_create(fa_action_null(), fa_milliseconds(100)),
        //                                         fa_pair_create(note1, fa_milliseconds(200)),
        //                                         fa_pair_create(note1, fa_milliseconds(100 + 100)),
        //                                         fa_pair_create(note1, fa_milliseconds(100))
        //                                     ));
        //
        // fa_action_t x = fa_action_repeat(fa_milliseconds(1000/5), note1);
        // fa_action_t y = fa_action_repeat(fa_milliseconds(1100/5), note2);

        fa_thread_sleep(1000);
        fa_midi_schedule_relative(seconds(0),

                                  fa_action_while(pred2, NULL,
                                                  fa_action_many(list(
                                                          fa_pair_create(fa_action_if(pred1, NULL, note1), fa_milliseconds(1000)),
                                                          fa_pair_create(fa_action_if(pred1, NULL, note1), fa_milliseconds(1000)),
                                                          fa_pair_create(fa_action_if(pred1, NULL,
                                                                  fa_action_while(pred2, NULL,
                                                                          fa_action_repeat(fa_milliseconds(1000), note2)
                                                                                 )), fa_milliseconds(1000))
                                                                 ))
                                                 )

                                  // fa_action_repeat(fa_milliseconds(200), fa_action_if(pred1, NULL,
                                  // fa_action_compose(note1, fa_action_do(_print, NULL))
                                  // ))

                                  // fa_action_compose3(
                                  // fa_action_repeat(fa_milliseconds(3000), fa_action_do(_print, NULL))
                                  // ,
                                  // fa_action_while(pred1, NULL, fa_action_repeat(seconds(1), note1))
                                  // ,
                                  // fa_action_while(pred2, NULL, fa_action_repeat(seconds(1), note2))
                                  // )

                                  , st);

        fa_thread_sleep(100000);
        // mark_used(notes1);
        // mark_used(notes2);
        // mark_used(x);
        // mark_used(y);
    }

    // {
    //     float h = 99;
    //     time_t half       = fa_milliseconds(h*2);
    //     time_t interv     = fa_milliseconds(h);
    //     fa_action_t note  = fa_action_send(string("midi"), fa_midi_message_create_simple(0x99, 60, 90));
    //     fa_action_t notes = fa_action_repeat(interv, note);
    //     fa_action_t off   = fa_action_send(string("midi"), fa_midi_message_create_simple(0x99, 61, 90));
    //     fa_action_t offs  = fa_action_repeat(interv, off);
    //
    //     fa_midi_schedule_relative(seconds(0), notes, st);
    //     fa_midi_schedule_relative(half,       offs, st);
    //     fa_thread_sleep(100000);
    //     mark_used(half);
    //     mark_used(notes);
    //     mark_used(offs);
    // }

    fa_destroy(st);
    fa_destroy(s);
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_initialize();

    run_midi();

    fa_terminate();
}
