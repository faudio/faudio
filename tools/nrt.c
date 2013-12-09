
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does runs a signal in non-realtime and writes
    the result to a file named 'test.wav'.

 */
// typedef fa_signal_t signal_t;

void helper_function()
{
    list_t actions = empty();

    for (int i = 0; i < 30; ++i) {
        actions = fa_list_dcons(
                      pair(hms(0, 0, i + 3), action_set(1, (i % 10) * 0.1)),
                      actions
                  );
    }

    signal_t r = fa_multiply(fa_signal_input(1), srandom());

    fa_signal_run_file(44100 * 300, actions, r, string("test.wav"));


    mark_used(actions);
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
