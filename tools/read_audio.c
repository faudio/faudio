
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program reads an audio file and prints its info.
 */

void read_audio()
{
    buffer_t b = fa_pair_second(fa_buffer_read_audio(string("test/test.wav")));

    fa_print_ln(fa_buffer_get_all_meta(b));
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();

    read_audio();

    fa_fa_terminate();
}
