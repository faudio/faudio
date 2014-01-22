
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */

void helper_function()
{
    fa_print_ln(string("This is fa_template!"));
}

int main(int argc, char const *argv[])
{
    fa_initialize();

    helper_function();

    fa_terminate();
}
