
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program reads a sound file and prints information about it.
 */

void read_and_print(string_t path)
{
    fa_print_ln(string_append(string("Reading file "), path));
    pair_t res = fa_buffer_read_audio(path);

    if (fa_error_check(res)) {
        fa_print("Error: Could not read file '%s'\n", path);
    } else {
        type_t   type    = fa_pair_first(res);
        buffer_t buffer  = fa_pair_second(res);

        fa_print("The type is: %s\n", type);
        fa_print("The size is: %s\n", i64(fa_buffer_size(buffer)));
    }
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();

    if (argc < 2) {
        fa_print_ln(string("Usage: fa_sndfile FILE"));
    } else {
        read_and_print(string((fa_string_utf8_t) argv[1]));
    }

    fa_fa_terminate();
}
