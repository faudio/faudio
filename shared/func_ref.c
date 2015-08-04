
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2015
    All rights reserved.

 */

#include <fa/func_ref.h>
// #include <fa/dynamic.h>
#include <fa/util.h>

struct _fa_func_ref_t {
    fa_impl_t      impl;
    void*          func;
    fa_ptr_t       data;
};

static int gFuncRefCount = 0;

// -----------------------------------------------------------------------------

inline static fa_func_ref_t new_func_ref(void* func, fa_ptr_t data)
{
    fa_ptr_t func_ref_impl(fa_id_t interface);

    fa_func_ref_t func_ref = fa_new(func_ref);
    func_ref->impl = &func_ref_impl;
    func_ref->func  = func;
    func_ref->data  = data;
    gFuncRefCount++;
    return func_ref;
}

inline static void delete_func_ref(fa_func_ref_t f)
{
    gFuncRefCount--;
    fa_delete(f);
}


// -----------------------------------------------------------------------------

fa_func_ref_t fa_func_ref_create(void* func, fa_ptr_t data)
{
    return new_func_ref(func, data);
}

void fa_func_ref_destroy(fa_func_ref_t func_ref)
{
    delete_func_ref(func_ref);
}

void* fa_func_ref_func(fa_func_ref_t func_ref)
{
    return func_ref->func;
}

fa_ptr_t fa_func_ref_data(fa_func_ref_t func_ref)
{
    return func_ref->data;
}

fa_func_ref_t fa_func_ref_copy(fa_func_ref_t func_ref)
{
    return fa_func_ref_create(func_ref->func, func_ref->data);
}

// --------------------------------------------------------------------------------

fa_ptr_t func_ref_copy(fa_ptr_t a)
{
    return fa_func_ref_copy(a);
}

fa_ptr_t func_ref_deep_copy(fa_ptr_t a)
{
    return fa_func_ref_copy(a); // nothing to deep-copy
}

fa_string_t func_ref_show(fa_ptr_t a)
{
    return fa_string_dappend(fa_string("<FuncRef "), fa_string_format_integral("%p>", (long) a));
}

void func_ref_destroy(fa_ptr_t a)
{
    fa_func_ref_destroy(a);
}

void func_ref_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    if (p(a)) fa_func_ref_destroy(a);
}

fa_ptr_t func_ref_impl(fa_id_t interface)
{
    static fa_copy_t func_ref_copy_impl        = { func_ref_copy, func_ref_deep_copy };
    static fa_string_show_t func_ref_show_impl = { func_ref_show };
    static fa_destroy_t func_ref_destroy_impl  = { func_ref_destroy, func_ref_deep_destroy };

    switch (interface) {
    case fa_copy_i:
        return &func_ref_copy_impl;
    case fa_string_show_i:
        return &func_ref_show_impl;
        
    case fa_destroy_i:
        return &func_ref_destroy_impl;
        
    default:
        return NULL;
    }
}

void fa_func_ref_log_count()
{
  fa_log_info(fa_string_dappend(fa_string("FuncRefs allocated: "), fa_string_dshow(fa_i32(gFuncRefCount))));
}
