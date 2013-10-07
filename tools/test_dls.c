
#include <fa/fa.h>
#include <fa/util.h>


#define RT 1

list_t just(ptr_t x, list_t xs)
{
    return x;
}

void run_dsl()
{                  
    if (RT)
    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);        
        list_t out           = fa_pair_to_list(fa_signal_dls());

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }
        
        for (int i = 0; true; ++i) {
            fa_audio_schedule(
                hms(0,0,0), 
                fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60+((i%12)*3), 90)), 
                st);
            fa_thread_sleep(100*1);
        }

        fa_destroy(st);
        fa_destroy(s);
    } else {
        fa_signal_run_file(44100*60, list(
            pair(
                hms(0,0,0), 
                fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60+((0%12)*3), 90))
                ),
            pair(
                hms(0,0,1), 
                fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60+((1%12)*3), 90))
                ),
            pair(
                hms(0,0,2), 
                fa_action_send(string("DLS"), fa_midi_message_create_simple(0x90, 60+((2%12)*3), 90))
                )
            
            ), 
            fa_pair_first(fa_signal_dls()), 
            string("test.wav"));
    }
    
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    
    run_dsl();
    
    fa_fa_terminate();
}
