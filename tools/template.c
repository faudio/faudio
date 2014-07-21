
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program does ...

 */

void helper_function()
{
    fa_print_ln(fa_string("This is fa_template!"));
}

int main(int argc, char const *argv[])
{
    fa_with_faudio() {
        helper_function();
    }
}
