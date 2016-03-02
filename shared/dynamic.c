
/*
    faudio

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
    if (!a) return null_type_repr;
    
    assert(fa_interface(fa_dynamic_i, a) && "Must implement Dynamic");

    return ((fa_dynamic_t *)
            fa_interface(fa_dynamic_i, a))->get_type(a);
}

// TODO: make this a part of the dynamic interface
fa_string_t fa_dynamic_type_name(fa_dynamic_type_repr_t type)
{
    switch(type) {
    case null_type_repr: return fa_string("null");
    case bool_type_repr: return fa_string("bool");
    case i8_type_repr: return fa_string("i8");
    case i16_type_repr: return fa_string("i16");
    case i32_type_repr: return fa_string("i32");
    case i64_type_repr: return fa_string("i64");
    case f32_type_repr: return fa_string("f32");
    case f64_type_repr: return fa_string("f64");
    case pair_type_repr: return fa_string("pair");
    case list_type_repr: return fa_string("list");
    case set_type_repr: return fa_string("set");
    case map_type_repr: return fa_string("map");
    case string_type_repr: return fa_string("string");
    case ratio_type_repr: return fa_string("ratio");
    case midi_message_type_repr: return fa_string("midi_message");
    case action_type_repr: return fa_string("action");
    case audio_stream_type_repr: return fa_string("audio_stream");
    case midi_stream_type_repr: return fa_string("midi_stream");
    case buffer_type_repr: return fa_string("buffer");
    case atomic_ring_buffer_type_repr: return fa_string("atomic_ring_buffer");
    case file_buffer_type_repr: return fa_string("file_buffer");
    default: return fa_string("unknown type");
    }
}