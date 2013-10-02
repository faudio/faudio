
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
void helper_function()
{
    list_t xs = list(
                    pair(i32(3), i32(30)),
                    pair(i32(2), i32(40)),
                    pair(i32(2), i32(45)),
                    pair(i32(4), i32(20)),
                    pair(i32(5), i32(10))
                );



    xs = fa_list_dmap(apply1, fa_pair_left_from_pair, xs);
    // xs = fa_list_dmap(apply1, fa_pair_swap, xs);
    xs = fa_list_dsort(xs);
    fa_print_ln(xs);
    mark_used(xs);
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
