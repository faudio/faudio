
#include <fae/fae.h>
#include <fae/util.h>

/*
    This program does ...
    
 */

void helper_function()
{
    fae_print_ln(string("This is fae_template!"));
}   

int main (int argc, char const *argv[])
{
    fae_fae_initialize();           
    helper_function();
    fae_fae_terminate();
}
