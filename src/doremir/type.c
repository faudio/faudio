
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/type.h>
#include <doremir/util.h>

doremir_type_t new_type(int tag)
{
    type_t t = malloc(sizeof(doremir_type_struct_t));
    t->impl = 0;
    t->tag = tag;
    return t;    
}            
void delete_type(doremir_type_t type)
{
    free(type);
}

// --------------------------------------------------------------------------------

doremir_type_t doremir_type_simple(doremir_type_simple_t type)
{
    type_t t = new_type(simple_type);
    t->fields.simple = type;
    return t;
}

doremir_type_t doremir_type_pair(doremir_type_t type1, doremir_type_t type2)
{
    type_t t = new_type(pair_type);
    t->fields.pair.fst = type1;
    t->fields.pair.snd = type2;
    return t;
}

doremir_type_t doremir_type_vector(doremir_type_t base, size_t size)
{
    type_t t = new_type(vector_type);
    t->fields.vector.base = base;
    t->fields.vector.size = size;
    return t;
}

doremir_type_t doremir_type_frame(doremir_type_t base)
{
    type_t t = new_type(frame_type);
    t->fields.frame.base = base;
    return t;
}

doremir_type_t doremir_type_copy(doremir_type_t type)
{                    
    type_t t = new_type(type->tag);    
    switch(type->tag)
    {
    case simple_type:
        t->fields.simple = type->fields.simple;
    case pair_type:     
        t->fields.pair.fst = type->fields.pair.fst;
        t->fields.pair.snd = type->fields.pair.snd;
    case vector_type:
        t->fields.vector.base = type->fields.vector.base;
        t->fields.vector.size = type->fields.vector.size;
    case frame_type:
        t->fields.frame.base = type->fields.frame.base;
    default:
        assert(false && "Missing label");
    }                  
    return t;
}

void doremir_type_destroy(doremir_type_t type)
{                                                    
    delete_type(type);
}

bool doremir_type_is_simple(doremir_type_t type)
{
    return type->tag == simple_type;
}                                   

bool doremir_type_is_pair(doremir_type_t type)
{
    return type->tag == pair_type;
}

bool doremir_type_is_vector(doremir_type_t type)
{
    return type->tag == vector_type;
}

bool doremir_type_is_frame(doremir_type_t type)
{
    return type->tag == frame_type;
}

// doremir_type_simple_t doremir_type_get_simple(doremir_type_t type)
// {
//     return type->fields.simple;
// }
// 
// doremir_pair_t doremir_type_get_pair(doremir_type_t type)
// {
//     return pair(type->fields.pair.fst, type->fields.pair.snd);
// }
// 
// doremir_pair_t doremir_type_get_vector(doremir_type_t type)
// {
//     return pair(type->fields.vector.base, type->fields.vector.size);
// }
// 
// doremir_type_t doremir_type_get_frame(doremir_type_t type)
// {               
//     return type->fields.vector.base;
// }


// --------------------------------------------------------------------------------

inline static 
size_t size_max(size_t a, size_t b)
{ 
    return (a > b) ? a : b; 
}       

inline static 
size_t pad(size_t x, size_t a)
{
  return (a - x) % a;
}

inline static 
size_t next_aligned(size_t x, size_t a)
{
  return x + pad(x, a);
}

inline static 
size_t simple_align(doremir_type_simple_t simple)
{
    switch (simple)
    {
    case uint8_type:
        return alignof(uint8_t);
    case double_type:
        return alignof(double);
    default:
        assert(false && "Missing label");
    }
}

inline static 
size_t simple_size(doremir_type_simple_t simple)
{
    switch (simple)
    {
    case uint8_type:
        return sizeof(uint8_t);
    case double_type:
        return sizeof(double);
    default:
        assert(false && "Missing label");
    }
}  

inline static
size_t align(doremir_type_t type)
{
    switch(type->tag)
    {
    case simple_type:
        return simple_align(type->fields.simple);
    case pair_type:
        return size_max(align(type->fields.pair.fst), align(type->fields.pair.snd));
    case vector_type:
        return align(type->fields.vector.base);
    case frame_type:
        return align(type->fields.frame.base);
    default:
        assert(false && "Missing label");
    }                                   
}

inline static
size_t size(doremir_type_frames_t frames, doremir_type_t type)
{
    switch(type->tag)
    {
    case simple_type:
        return simple_size(type->fields.simple);
    case pair_type:
        return next_aligned(size(frames, type->fields.pair.fst), align(type->fields.pair.snd));
    case vector_type:
        return size(frames, type->fields.vector.base) * type->fields.vector.size;
    case frame_type:
        return size(frames, type->fields.frame.base) * frames;

    default:
        assert(false && "Missing label");
    }                  
}


size_t doremir_type_size_of(doremir_type_frames_t frames, doremir_type_t type)
{
    return size(frames, type);
}
size_t doremir_type_align_of(doremir_type_t type)
{
    return align(type);
}


// --------------------------------------------------------------------------------