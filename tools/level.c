
#include <fa/fa.h>
#include <fa/util.h>
#include "../shared/signal.h"
#include "common.h"

/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */


#define RT 1
#define kThisPlugOffset 32 // TODO
#define kInputOffset 8 // FIXME
#define kBarsInMeter 40

int processed = 0;
static bool should_send = false;
double values[2];


/*
    (defprocessor slope (curr)
      (recur (rec)
         (max curr
           (+ (* 0.999 rec)
              (* 0.001 curr)))))
*/
signal_t slope_(signal_t curr, signal_t prev)
{
    return
        fa_signal_max(
            fa_signal_add(
                fa_signal_multiply(
                    fa_constant(0.999),
                    prev),
                fa_signal_multiply(
                    fa_constant(0.001),
                    curr)
            ),
            curr);
}
signal_t slope(signal_t x)
{
    return fa_signal_loop((fa_signal_unary_signal_t)slope_, x);
}


static string_t out_name;
static fa_ptr_t    out_value;
fa_ptr_t before_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    out_name  = fa_string("foo");
    out_value = i32(1);
    return x;
}
fa_ptr_t after_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    fa_destroy(out_name);
    fa_destroy(out_value);
    return x;
}
fa_ptr_t render_(fa_ptr_t x, int count, fa_signal_state_t *state)
{
    if (!kVectorMode) {
        values[0] = state->buffer[(kThisPlugOffset + 0) * kMaxVectorSize];
        values[1] = state->buffer[(kThisPlugOffset + 1) * kMaxVectorSize];
        should_send = should_send || (!(state->count % (44100 / 100)));
    } else {
        values[0] = state->buffer[(kThisPlugOffset + 0) * kMaxVectorSize + 0];
        values[1] = state->buffer[(kThisPlugOffset + 1) * kMaxVectorSize + 0];
        // TODO should_send
    }


    return x;
}

fa_ptr_t receive_(fa_ptr_t x, fa_signal_name_t n, fa_signal_message_t msg)
{
    // should_send = true;
    return x;
}

fa_ptr_t send_(fa_ptr_t x, fa_signal_message_callback_t cb, fa_ptr_t data)
{
    // TODO should not allocate here
    // The value passed is *not* destroyed, but *will* be copied
    // All create/destroy should happen in the setup phase
    if (should_send) {
        should_send = false;
        cb(data, out_name, f32(values[0]));
    }

    return x;
}


pair_t fa_signal_level(signal_t a, signal_t b)
{
    fa_signal_custom_processor_t *proc = fa_malloc(sizeof(fa_signal_custom_processor_t));
    proc->before  = before_;
    proc->after   = after_;
    proc->render  = render_;
    proc->receive = receive_;
    proc->send    = send_;
    proc->destroy = NULL;
    proc->data    = NULL;

    return fa_pair_create(fa_signal_custom(proc,
                                           fa_signal_latter(fa_signal_output(0, kThisPlugOffset + 0, slope(fa_signal_absolute(a))), a)
                                          ),
                          fa_signal_latter(fa_signal_output(0, kThisPlugOffset + 1, b), b)
                         );
}




// amp2db x = logBase (10/1) x * 10
double amp2db(double x)
{
    return log10(x) * 10;
}

fa_ptr_t _message_out(fa_ptr_t x, fa_ptr_t name, fa_ptr_t value)
{
    // fa_print("Receieved 2: %s\n", fa_pair_create(name, value));
    int bars = (int)(((amp2db(fa_peek_double(value)) + 20) / 20) * kBarsInMeter);


    for (int i = 0; i < kBarsInMeter; ++i) {
        if (i < bars) {
            printf("#");
        } else {
            printf(" ");
        }
    }

    printf("|");

    printf("\r");
    fflush(stdout);

    return x;
}
void run_level()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out           = fa_pair_to_list(fa_signal_level(
                                                   // fa_signal_sin(fa_signal_line(0.1)),
                                                   fa_signal_input(kInputOffset + 0),

                                                   fa_signal_input(kInputOffset + 0)));

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);
        // fa_audio_add_message_callback(_message_out, NULL, st);
        fa_audio_add_message_callback(_message_out, NULL, st);
        // printf("                                                  |     |    |    |    |\n");

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        fa_thread_sleep(300000);
        fa_destroy(st);
        fa_destroy(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {

        run_level();
    }
}
