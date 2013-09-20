
#include <fae/fae.h>
#include <fae/util.h>

/*
    This program reads a sound file and prints information about it.
 */

void read_and_print(string_t path)
{
    fae_print_ln(string_append(string("Reading file "), path));
    pair_t res = fae_buffer_read_audio(path);

    if (fae_error_check(res)) {
        fae_print("Error: Could not read file '%s'\n", path);
    } else {
        type_t   type    = fae_pair_first(res);
        buffer_t buffer  = fae_pair_second(res);

        fae_print("The type is: %s\n", type);
        fae_print("The size is: %s\n", i64(fae_buffer_size(buffer)));
    }
}

int main(int argc, char const *argv[])
{
    fae_fae_initialize();

    if (argc < 2) {
        fae_print_ln(string("Usage: fae_sndfile FILE"));
    } else {
        read_and_print(string((fae_string_utf8_t) argv[1]));
    }

    fae_fae_terminate();
}

