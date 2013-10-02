
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program prints the version of faudio.
 */

void helper_function()
{
    fa_print_ln(fa_fa_version_string());
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
