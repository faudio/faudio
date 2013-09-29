
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
// typedef fa_signal_t signal_t;

#define N           (44100*60)
#define PI          3.1415
#define TAU         (2 * PI)

#define add_        fa_signal_add
#define mul_        fa_signal_multiply
#define sin_        fa_signal_sin
#define cos_        fa_signal_cos
#define time_       fa_signal_time
#define random_     fa_signal_random
#define const_      fa_signal_constant
#define imp_        fa_signal_impulse
#define line_       fa_signal_line
#define delay_      fa_signal_delay
#define loop_       fa_signal_loop
#define input_      fa_signal_input
#define output_     fa_signal_output

list_t just(ptr_t x, list_t xs)
{
    return x;
}

signal_t add1(ptr_t _, signal_t x)
{
    return add_(x, const_(1));
}


void helper_function(string_t path)
{
    {                    
        buffer_t buf;
        pair_t res = fa_buffer_read_audio(path);
        if (fa_error_check(res)) {
            fa_print("Error: Could not read file '%s'\n", path);
            exit(-1);
        } else {
            buf = fa_pair_second(res);
        }
        
        signal_t j  = add_(loop_(add1, NULL), const_(-1));  // 0,1,2,3 ...
        signal_t li = add_(mul_(j, const_(2)), const_(0));
        signal_t ri = add_(mul_(j, const_(2)), const_(1));

        signal_t l = fa_signal_play(buf, li);
        signal_t r = fa_signal_play(buf, ri);

        // fa_print_ln(l);
        // fa_signal_print(200, li);
        // exit(0);

        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, list(l,r));
        
        if (fa_check(st)) {
            fa_error_log(st, NULL);
        } else {
            while (1) {
                fa_thread_sleep(10000);
            }
        }
        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    // fa_fa_set_log_std();
    fa_fa_initialize();

    if (argc < 2) {
        fa_print_ln(string("Usage: fa_play FILE"));
    } else {
        helper_function(string((fa_string_utf8_t) argv[1]));
    }

    fa_fa_terminate();
}
