
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/type.h>
#include <fa/util.h>

typedef fa_type_simple_t simple_t;

struct _fa_type_t {

    impl_t                  impl;           

    enum {
        simple_type,
        pair_type,
        vector_type,
        frame_type
    }                       tag;

    union {
        simple_t            simple;         //  to, i16 ... ptr

        struct {
            ptr_t           fst;
            ptr_t           snd;
        }                   pair;           //  (a,b)
        struct {
            ptr_t           base;
            size_t          size;
        }                   vector;         //  [a x n]
        struct {
            ptr_t           base;
        }                   frame;          //  {a}

    }                       fields;
};

// --------------------------------------------------------------------------------

type_t new_type(int tag)
{
    type_t t = fa_new(type);

    fa_ptr_t type_impl(fa_id_t interface);
    t->impl = &type_impl;
    t->tag  = tag;

    return t;
}

void delete_type(fa_type_t type)
{
    // TODO manage components
    fa_delete(type);
}

#define is_simple(v)    (v->tag == simple_type)
#define is_pair(v)      (v->tag == pair_type)
#define is_vector(v)    (v->tag == vector_type)
#define is_frame(v)     (v->tag == frame_type)

#define simple_get(v)   v->fields.simple
#define pair_get(v,f)   v->fields.pair.f
#define vector_get(v,f) v->fields.vector.f
#define frame_get(v,f)  v->fields.frame.f


// --------------------------------------------------------------------------------

// TODO manage component types

/** Create a representation of a simple type.
 */
fa_type_t fa_type_simple(fa_type_simple_t type)
{
    type_t t = new_type(simple_type);

    simple_get(t) = type;

    return t;
}

/** Create a representation of a pair type.
 */
fa_type_t fa_type_pair(fa_type_t type1, fa_type_t type2)
{
    type_t t = new_type(pair_type);

    pair_get(t, fst) = type1;
    pair_get(t, snd) = type2;

    return t;
}

/** Create a representation of a vector type.
 */
fa_type_t fa_type_vector(fa_type_t base, size_t size)
{
    type_t t = new_type(vector_type);

    vector_get(t, base) = base;
    vector_get(t, size) = size;

    return t;
}

/** Create a representation of a frame type.
 */
fa_type_t fa_type_frame(fa_type_t base)
{
    type_t t = new_type(frame_type);

    frame_get(t, base) = base;

    return t;
}

/** Copy the given type representation.
 */
fa_type_t fa_type_copy(fa_type_t type)
{
    type_t t = new_type(type->tag);

    switch (type->tag) {
    case simple_type:
        simple_get(t) = simple_get(type);
        break;

    case pair_type:
        pair_get(t, fst) = pair_get(type, fst);
        pair_get(t, snd) = pair_get(type, snd);
        break;

    case vector_type:
        vector_get(t, base) = vector_get(type, base);
        vector_get(t, size) = vector_get(type, size);
        break;

    case frame_type:
        frame_get(t, base) = frame_get(type, base);
        break;

    default:
        assert(false && "Missing label");
    }

    return t;
}

/** Destroy the given type representation.
 */
void fa_type_destroy(fa_type_t type)
{
    delete_type(type);
}

/** Create a vector type by repeating the given value.
 */
fa_type_t fa_type_repeat(int times, fa_type_t type)
{
    if (times == 0) {
        return type(unit);
    } else if (times == 1) {
        return fa_copy(type);
    } else {
        return type_pair(type, fa_type_repeat(times - 1, type));
    }
}

/** Whether the type represented by the given value is simple.
 */
bool fa_type_is_simple(fa_type_t type)
{
    return type->tag == simple_type;
}

/** Whether the type represented by the given value is a pair type.
 */
bool fa_type_is_pair(fa_type_t type)
{
    return type->tag == pair_type;
}

/** Whether the type represented by the given value is a vector type.
 */
bool fa_type_is_vector(fa_type_t type)
{
    return type->tag == vector_type;
}

/** Whether the type represented by the given value is a frame type.
 */
bool fa_type_is_frame(fa_type_t type)
{
    return type->tag == frame_type;
}

/** Return the simple type.
 */
fa_type_simple_t fa_type_get_simple(fa_type_t t)
{
    assert(is_simple(t) && "Not a simple type");
    return simple_get(t);
}

/** Return the first component of a pair type.
 */
fa_type_t fa_type_get_pair_fst(fa_type_t t)
{
    assert(is_pair(t) && "Not a pair type");
    return pair_get(t, fst);
}

/** Return the second component of a pair type.
 */
fa_type_t fa_type_get_pair_snd(fa_type_t t)
{
    assert(is_pair(t) && "Not a pair type");
    return pair_get(t, snd);
}

/** Return the base of a vector type.
 */
fa_type_t fa_type_get_vector_base(fa_type_t t)
{
    assert(is_vector(t) && "Not a vector type");
    return vector_get(t, base);
}

/** Return the size of a vector type.
 */
size_t fa_type_get_vector_size(fa_type_t t)
{
    assert(is_vector(t) && "Not a vector type");
    return vector_get(t, size);
}

/** Return the base of a frame type.
 */
fa_type_t fa_type_get_frame_base(fa_type_t t)
{
    assert(is_frame(t) && "Not a frame type");
    return frame_get(t, base);
}


// --------------------------------------------------------------------------------

inline static size_t pad(size_t x, size_t a)
{
    return (a - x) % a;
}

inline static size_t next_aligned(size_t x, size_t a)
{
    return x + pad(x, a);
}

inline static size_t simple_align(fa_type_simple_t simple)
{
    match(simple) {
        against(unit_type) 0;
        against(i8_type)   alignof(uint8_t);
        against(i16_type)  alignof(uint16_t);
        against(i32_type)  alignof(uint32_t);
        against(i64_type)  alignof(uint64_t);
        against(f32_type)  alignof(float);
        against(f64_type)  alignof(double);
        against(ptr_type)  alignof(ptr_t);
        no_default();
    }
}

inline static size_t simple_size(fa_type_simple_t simple)
{
    match(simple) {
        against(unit_type) 0;
        against(i8_type)   sizeof(uint8_t);
        against(i16_type)  sizeof(uint16_t);
        against(i32_type)  sizeof(uint32_t);
        against(i64_type)  sizeof(uint64_t);
        against(f32_type)  sizeof(float);
        against(f64_type)  sizeof(double);
        against(ptr_type)  sizeof(ptr_t);
        no_default();
    }
}

inline static size_t align(fa_type_t type)
{
    match(type->tag) {
        against(simple_type)  simple_align(simple_get(type));
        against(pair_type)    size_max(align(pair_get(type, fst)), align(pair_get(type, snd)));
        against(vector_type)  align(vector_get(type, base));
        against(frame_type)   align(frame_get(type, base));
        no_default();
    }
}

inline static size_t pair_size(size_t frames, fa_type_t type);
inline static size_t size(fa_type_frames_t frames, fa_type_t type);


inline static size_t pair_size(size_t frames, fa_type_t type)
{
    size_t offset = next_aligned(size(frames, pair_get(type, fst)), align(pair_get(type, snd)));
    return next_aligned(offset + size(frames, pair_get(type, snd)), align(type));
}


inline static size_t size(fa_type_frames_t frames, fa_type_t type)
{
    match(type->tag) {
        against(simple_type)    simple_size(simple_get(type));
        against(pair_type)      pair_size(frames, type);
        against(vector_type)    size(frames, vector_get(type, base)) * vector_get(type, size);
        against(frame_type)     size(frames, frame_get(type, base)) * frames;
        no_default();
    }
}

// TODO pair_size should use this
inline static size_t offset(fa_type_frames_t frames, fa_type_t type)
{
    switch (type->tag) {
    case pair_type:
        return next_aligned(size(frames, pair_get(type, fst)), align(pair_get(type, snd)));

    case simple_type:
    case vector_type:
    case frame_type:
        return 0;

    default:
        assert(false && "Missing label");
    }
}

inline static int channels(fa_type_t type)
{
    switch (type->tag) {
    case simple_type:
        return simple_get(type) == unit_type ? 0 : 1;

    case frame_type:
        return 1;

    case pair_type:
        return channels(pair_get(type, fst)) + channels(pair_get(type, snd));

    case vector_type:
        return channels(vector_get(type, base)) * vector_get(type, size);

    default:
        assert(false && "Missing label");
    }
}

/** Return the number of channels in the given type.
 */
int fa_type_channels(fa_type_t type)
{
    return channels(type);
}

/**
    Return the size of the represented type.
 */
size_t fa_type_size_of(fa_type_frames_t frames, fa_type_t type)
{
    return size(frames, type);
}

/**
    Return the alignment of the represented type.
 */
size_t fa_type_align_of(fa_type_t type)
{
    return align(type);
}

/**
    Return the offset of the second element in the represented type.
    If the given type is not a pair type, return 0.
 */
size_t fa_type_offset_of(fa_type_frames_t frames, fa_type_t type)
{
    return offset(frames, type);
}


// --------------------------------------------------------------------------------

bool type_equal(fa_ptr_t a, fa_ptr_t b)
{
    type_t c = (type_t) a;
    type_t d = (type_t) b;

    if (c->tag != d->tag) {
        return false;
    }

    switch (c->tag) {
    case simple_type:
        return simple_get(c) == simple_get(d);

    case pair_type:
        return type_equal(pair_get(c, fst), pair_get(d, fst))
               && type_equal(pair_get(c, snd), pair_get(d, snd));

    case vector_type:
        return type_equal(vector_get(c, base), vector_get(d, base))
               && vector_get(c, size) == vector_get(d, size);

    case frame_type:
        return type_equal(frame_get(c, base), frame_get(d, base));

    default:
        assert(false && "Missing label");
    }
}

inline static string_t simple_show(fa_type_simple_t simple)
{
    switch (simple) {
    case unit_type:
        return string("()");

    case i8_type:
        return string("i8");

    case i16_type:
        return string("i16");

    case i32_type:
        return string("i32");

    case i64_type:
        return string("i64");

    case f32_type:
        return string("f32");

    case f64_type:
        return string("f64");

    case ptr_type:
        return string("ptr");

    default:
        assert(false && "Missing label");
    }
}

string_t type_show(fa_ptr_t a)
{
    type_t type = (type_t) a;
    string_t s = string("");

    switch (type->tag) {
    case simple_type:
        s = string_dappend(s, simple_show(simple_get(type)));
        return s;

    case pair_type:
        s = string_dappend(s, string("("));
        s = string_dappend(s, type_show(pair_get(type, fst)));
        s = string_dappend(s, string(","));
        s = string_dappend(s, type_show(pair_get(type, snd)));
        s = string_dappend(s, string(")"));
        return s;

    case vector_type:
        s = string_dappend(s, string("["));
        s = string_dappend(s, type_show(vector_get(type, base)));
        s = string_dappend(s, string(" x "));
        s = string_dappend(s, format_integral("%i", vector_get(type, size)));
        s = string_dappend(s, string("]"));
        return s;

    case frame_type:
        s = string_dappend(s, string("{"));
        s = string_dappend(s, type_show(vector_get(type, base)));
        s = string_dappend(s, string("}"));
        return s;

    default:
        assert(false && "Missing label");
    }
}

fa_ptr_t type_copy(fa_ptr_t a)
{
    return fa_type_copy(a);
}

void type_destroy(fa_ptr_t a)
{
    fa_type_destroy(a);
}

fa_ptr_t type_impl(fa_id_t interface)
{
    static fa_equal_t type_equal_impl = { type_equal };
    static fa_string_show_t type_show_impl = { type_show };
    static fa_copy_t type_copy_impl = { type_copy };
    static fa_destroy_t type_destroy_impl = { type_destroy };

    switch (interface) {
    case fa_equal_i:
        return &type_equal_impl;

    case fa_string_show_i:
        return &type_show_impl;

    case fa_copy_i:
        return &type_copy_impl;

    case fa_destroy_i:
        return &type_destroy_impl;

    default:
        return NULL;
    }
}

