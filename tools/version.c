
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program prints the version of faudio.
 */

void print_version()
{
    fa_print_ln(fa_version_string());
}

int main(int argc, char const *argv[])
{
    fa_initialize();

    print_version();

    fa_terminate();
}
