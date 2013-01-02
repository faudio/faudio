
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/time.h>
#include <doremir/util.h>

typedef doremir_time_unit_t unit_t;

struct _doremir_time_t {         
        doremir_impl_t      impl;       /* Interface dispatcher */
        doremir_ratio_t     value; 
        doremir_time_unit_t unit;
};


/**
    Print the time as an ISO 8601 duration, possibly losing precision.

    For example `P0000-00-00T00:01:24.3333` for 1 minute and 24 1/3 seconds.
 */
doremir_string_t doremir_time_to_iso(doremir_time_t time)
{
    
}
