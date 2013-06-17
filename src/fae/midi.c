
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/midi.h>
#include <fae/util.h>

typedef fae_midi_status_t   status_t;
typedef fae_midi_data_t     data_t;

struct _fae_midi_t {
    impl_t                  impl;           //  Interface dispatcher
    bool                    is_sysex;       //  Whether it is a sysex message
    union {                                 //  Status and data
        uint8_t             simple[3];
        fae_buffer_t    sysex;
    } data;
};

#define is_simple(x) (!x->is_sysex)
#define is_sysex(x)  (x->is_sysex)


// --------------------------------------------------------------------------------

inline static fae_midi_t new_midi()
{
    fae_ptr_t midi_impl(fae_id_t interface);

    fae_midi_t t = fae_new(midi);
    t->impl  = &midi_impl;
    return t;
}

void delete_midi(fae_midi_t midi)
{
    fae_delete(midi);
}


// --------------------------------------------------------------------------------

/** Creates a simple message from the given components.
    @param status   The status byte.
    @param data1    The first data byte.
    @param data2    The second data byte.
    @return         A new Midi message.
 */
fae_midi_t fae_midi_create_simple(status_t status,
                                          data_t data1,
                                          data_t data2)
{
    assert(status != 0xf0 && status != 0xf7);

    fae_midi_t m = new_midi();

    m->is_sysex = false;
    char simp[3] = { status, data1, data2 };
    memcpy(&m->data.simple, simp, 3);

    return m;
}

/** Creates a sysex message from the given data buffer (not including F0 and F7).
    @param data     Raw data buffer (transfered).
    @return         A new sysex message.
 */
fae_midi_t fae_midi_create_sysex(fae_buffer_t data)
{
    fae_midi_t m = new_midi();
    m->is_sysex = true;
    m->data.sysex = data;
    return m;
}

/** Copy the given midi message.
 */
fae_midi_t fae_midi_copy(fae_midi_t midi)
{
    fae_midi_t m = new_midi();
    m->is_sysex = midi->is_sysex;

    if (!midi->is_sysex) {
        memcpy(m->data.simple, midi->data.simple, 3);
    } else {
        m->data.sysex = fae_copy(midi->data.sysex);
    }

    return m;
}

/** Destroy the given midi message.
 */
void fae_midi_destroy(fae_midi_t midi)
{
    if (midi->is_sysex) {
        fae_destroy(midi->data.sysex);
    }

    delete_midi(midi);
}

/** Return the status byte of given midi message.
 */
bool fae_midi_is_simple(fae_midi_t midi)
{
    return !midi->is_sysex;
}

/** Return whether the given midi message is a sysex message.
 */
bool fae_midi_is_sysex(fae_midi_t midi)
{
    return midi->is_sysex;
}

/** Return the status byte of given midi message.
 */
fae_midi_status_t fae_midi_status(fae_midi_t midi)
{
    assert(is_simple(midi) && "Not a simple message");
    return midi->data.simple[0] & 0xf0;
}

/** Return the channel byte of given midi message.
 */
fae_midi_channel_t fae_midi_channel(fae_midi_t midi)
{
    assert(is_simple(midi) && "Not a simple message");
    return midi->data.simple[0] & 0x0f;
}

/** Return whether the given midi message is a non-sysex message.
 */
fae_pair_t fae_midi_simple_data(fae_midi_t midi)
{
    assert(is_simple(midi) && "Not a simple message");
    return fae_pair_create(i8(midi->data.simple[1]), i8(midi->data.simple[2]));
}

/** Return the data buffer of a sysex message, except for the wrapping `F0` and `F7` bytes.
 */
fae_buffer_t fae_midi_sysex_data(fae_midi_t midi)
{
    assert(is_sysex(midi) && "Not a sysex message");
    return midi->data.sysex;
}

#define midi_wrap(status, data1, data2)   \
         ((((data2) << 16) & 0xFF0000) |  \
          (((data1) << 8) & 0xFF00) |     \
          ((status) & 0xFF))

long fae_midi_simple_to_long(fae_midi_t midi)
{
    assert(is_simple(midi) && "Not a simple message");
    return midi_wrap(midi->data.simple[0], midi->data.simple[1], midi->data.simple[2]);
}


// --------------------------------------------------------------------------------

bool midi_equal(fae_ptr_t a, fae_ptr_t b)
{
    midi_t midi1 = (midi_t) a;
    midi_t midi2 = (midi_t) b;

    if (midi1->is_sysex != midi2->is_sysex) {
        return false;
    }

    if (!midi1->is_sysex) {
        return memcmp(&midi1->data.simple, &midi2->data.simple, 3) == 0;
    } else {
        return fae_equal(midi1->data.sysex, midi2->data.sysex);
    }
}

// Note: We arbitrarily define simple < sysex

bool midi_less_than(fae_ptr_t a, fae_ptr_t b)
{
    midi_t midi1 = (midi_t) a;
    midi_t midi2 = (midi_t) b;

    if (midi1->is_sysex != midi2->is_sysex) {
        return midi1->is_sysex;
    }

    if (!midi1->is_sysex) {
        return memcmp(&midi1->data.simple, &midi2->data.simple, 3) < 0;
    } else {
        return fae_less_than(midi1->data.sysex, midi2->data.sysex);
    }
}

bool midi_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    midi_t midi1 = (midi_t) a;
    midi_t midi2 = (midi_t) b;

    if (midi1->is_sysex != midi2->is_sysex) {
        return midi2->is_sysex;
    }

    if (!midi1->is_sysex) {
        return memcmp(&midi1->data.simple, &midi2->data.simple, 3) > 0;
    } else {
        return fae_greater_than(midi1->data.sysex, midi2->data.sysex);
    }
}

fae_string_t midi_show(fae_ptr_t a)
{
    fae_midi_t midi = (fae_midi_t) a;
    string_t s = string("<Midi");

    if (!midi->is_sysex) {
        s = string_dappend(s, fae_string_format_integral(" %02x", midi->data.simple[0]));
        s = string_dappend(s, fae_string_format_integral(" %02x", midi->data.simple[1]));
        s = string_dappend(s, fae_string_format_integral(" %02x", midi->data.simple[2]));
    } else {
        // TODO dump without <Buffer > wrap
        s = string_dappend(s, string(" SysEx "));
        s = string_dappend(s, fae_string_show(midi->data.sysex));
        s = string_dappend(s, string(" "));
    }

    s = string_dappend(s, string(">"));
    return s;
}

fae_ptr_t midi_copy(fae_ptr_t a)
{
    return fae_midi_copy(a);
}

void midi_destroy(fae_ptr_t a)
{
    fae_midi_destroy(a);
}


fae_ptr_t midi_impl(fae_id_t interface)
{
    static fae_equal_t midi_equal_impl = { midi_equal };
    static fae_order_t midi_order_impl = { midi_less_than, midi_greater_than };
    static fae_string_show_t midi_show_impl = { midi_show };
    static fae_copy_t midi_copy_impl = { midi_copy };
    static fae_destroy_t midi_destroy_impl = { midi_destroy };

    switch (interface) {
    case fae_equal_i:
        return &midi_equal_impl;

    case fae_order_i:
        return &midi_order_impl;

    case fae_string_show_i:
        return &midi_show_impl;

    case fae_copy_i:
        return &midi_copy_impl;

    case fae_destroy_i:
        return &midi_destroy_impl;

    default:
        return NULL;
    }
}

