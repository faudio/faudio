
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa.h>
#include <fa/dynamic.h>
#include <fa/util.h>

bool fa_dynamic_check(fa_ptr_t a)
{
    return fa_interface(fa_dynamic_i, a);
}

fa_dynamic_type_repr_t fa_dynamic_get_type(fa_ptr_t a)
{
    assert(fa_interface(fa_dynamic_i, a) && "Must implement Dynamic");

    return ((fa_dynamic_t *) 
        fa_interface(fa_dynamic_i, a))->get_type(a);
}

