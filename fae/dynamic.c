
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae.h>
#include <fae/dynamic.h>
#include <fae/util.h>

bool fae_dynamic_check(fae_ptr_t a)
{
    return fae_interface(fae_dynamic_i, a);
}

fae_dynamic_type_repr_t fae_dynamic_get_type(fae_ptr_t a)
{
    assert(fae_interface(fae_dynamic_i, a) && "Must implement Dynamic");

    return ((fae_dynamic_t *) 
        fae_interface(fae_dynamic_i, a))->get_type(a);
}

