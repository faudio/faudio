
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/device/file.h>
#include <doremir/thread.h>
#include <doremir/error.h>
#include <doremir/util.h>
#include <sndfile.h>

#include "../processor/vm.h"

typedef doremir_device_file_t                  device_t;
typedef doremir_device_file_result_t           result_t;

struct _doremir_device_file_t {
    impl_t              impl;
    string_t            path;
    SNDFILE            *file;
};

struct _doremir_device_file_result_t {

    device_t            input, output;
    processor_t         proc;
    proc_interface_t   *proc_impl;
    int32_t             time;
    dispatcher_t        in_disp;
    dispatcher_t        out_disp;

};

file_device_t doremir_device_file_open(string_t path)
{
    inform(string_dappend(string("Opening file stream "), doremir_string_copy(path)));

    {
        char err[100];
        snprintf(err, 100, "Could not read audio file '%s'", unstring(path));
        return doremir_error_create_simple(error, string(err), string("Doremir.Buffer"));
    }
}

void doremir_device_file_close(file_device_t device)
{
    inform(string_dappend(string("Closing file stream "), doremir_string_copy(device->path)));
}

doremir_device_file_result_t doremir_device_file_run(
    file_device_t input,
    processor_t   processor,
    file_device_t output)
{
    // create result_t
    // set input, output, proc, proc_impl

    // check types

    // Allocate VM
    lmm_t vm = lmm_create();
    lmm_alloc(vm, 1024, 0);

    // call before
    // run processing loop
    // call after

    // Free VM
    lmm_destroy(vm);
}

