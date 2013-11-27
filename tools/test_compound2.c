
#include <fa/fa.h>
#define NO_THREAD_T
#include <fa/util.h>
#undef NO_THREAD_T

#include <ApplicationServices/ApplicationServices.h> // DEBUG


list_t just(ptr_t x, list_t xs)
{
    return x;
}

bool pred1(ptr_t _, ptr_t x) {
    
    CGEventRef event = CGEventCreate(nil);
    CGPoint loc = CGEventGetLocation(event);
    inform(fa_string_format_floating("x: %f", loc.x));
    bool res = loc.x > 200;
    CFRelease(event);
    
    return res;
}

bool pred2(ptr_t _, ptr_t x) {
    
    CGEventRef event = CGEventCreate(nil);
    CGPoint loc = CGEventGetLocation(event);
    inform(fa_string_format_floating("x: %f", loc.x));
    bool res = loc.y > 200;
    CFRelease(event);
    
    return res;
}

void run_test()
{
    fa_midi_session_t s = fa_midi_begin_session();
    // fa_midi_device_t i  = fa_midi_default_input(s);

    fa_midi_device_t o  = fa_midi_default_output(s);
    assert(o && "No output");

    // fa_midi_stream_t st = fa_midi_open_stream(o);
    // if (fa_check(st)) {
        // fa_error_log(st, NULL);
    // }

    fa_audio_stream_t ast = NULL;

    {
        fa_audio_session_t as = fa_audio_begin_session();
        fa_audio_device_t ai = fa_audio_default_input(as);
        fa_audio_device_t ao = fa_audio_default_output(as);
        ast = fa_audio_open_stream(ai, ao, just, fa_pair_to_list(fa_signal_dls()));
        fa_print_ln(ast);
        
        mark_used(ast);
                                
        // Use audio clock
        // fa_midi_set_clock(st, fa_audio_stream_clock(ast));
    }
                        
    {               
        // 201 109 !!!
        int j = 100;
        int k = j*2;

        fa_action_t note1  = fa_action_send(string("midi"), fa_midi_message_create_simple(0x99, 60, 80));
        fa_action_t note2  = fa_action_send(string("midi"), fa_midi_message_create_simple(0x99, 61, 80));

        fa_action_t notes1 = fa_action_many(fa_list_join(list(
            list(
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k-j)),
                pair(note1,             fa_milliseconds(j))
                ),
            list(
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k-j)),
                pair(note1,             fa_milliseconds(j))
                ),
            list(
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k-j)),
                pair(note1,             fa_milliseconds(j))
                ),
            list(
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k-j)),
                pair(note1,             fa_milliseconds(j))
                ),
            list(
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k)),
                pair(note1,             fa_milliseconds(k-j)),
                pair(note1,             fa_milliseconds(j))
                )
            )));
        fa_action_t notes2 = fa_action_many(list(
                pair(note2,             fa_milliseconds(k*3)),
                pair(note2,             fa_milliseconds(k*3)),
                pair(note2,             fa_milliseconds(k*3)),
                pair(note2,             fa_milliseconds(k*3)),
                pair(note2,             fa_milliseconds(k*3))
            ));
        
        // fa_audio_schedule_relative(seconds(0), notes1, ast);
        // fa_audio_schedule_relative(seconds(0), notes2, ast);

        // fa_audio_schedule_relative(seconds(0), fa_action_repeat(fa_milliseconds(3000), 
        //     fa_action_many(list(
        //     pair(notes1, fa_milliseconds(0)), 
        //     pair(notes2, fa_milliseconds(0)) 
        //     ))), ast);

        // 20 BE
        
        int n = 1000;
        fa_audio_schedule_relative(seconds(0), fa_action_repeat(fa_milliseconds(n), note1), ast);
        fa_audio_schedule_relative(seconds(0), fa_action_repeat(fa_milliseconds(n*2), note2), ast);
        // 

        fa_thread_sleep(100000);
        mark_used(notes1);
        mark_used(notes2);
    }

    fa_destroy(ast);
    // fa_destroy(st);
    fa_destroy(s);
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();

    run_test();

    fa_fa_terminate();
}
