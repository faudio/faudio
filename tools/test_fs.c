
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */


void run_fs()
{
    fa_print_ln(string("This is fa_template!"));
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();

    run_fs();

    fa_fa_terminate();
}
