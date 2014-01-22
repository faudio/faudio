
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program creates and opens a plot of the given audio file.
 */

void plot_file(string_t path)
{
    pair_t res = fa_buffer_read_audio(path);

    if (fa_error_check(res)) {
        fa_error_log("Could not plot file!", (error_t) res);
        return;
    }

    buffer_t buf = fa_pair_second(res);
    fa_plot_buffer_double(buf, NULL, NULL);
    fa_destroy(buf);
    fa_destroy(res);
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    if (argc < 2) {
        fa_print_ln(string("Usage: fa_plot_file FILE"));
    } else {
        plot_file(string((fa_string_utf8_t) argv[1]));
    }

    fa_terminate();
}
