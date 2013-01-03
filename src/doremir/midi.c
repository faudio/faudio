
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/midi.h>
#include <doremir/util.h>

doremir_ptr_t midi_impl(doremir_id_t interface);

typedef doremir_midi_status_t status_t;
typedef doremir_midi_data_t data_t;

struct _doremir_midi_t {
        doremir_impl_t          impl;           /* Interface dispatcher */
        bool                    is_sysex;       /* Whether it is a sysex message */
        union {                                 /* Data sink */
            uint8_t             simple[3];
            doremir_buffer_t    sysex;
        } data;
};

inline static doremir_midi_t
new_midi()
{
    doremir_midi_t t = doremir_new(midi);
    t->impl  = &midi_impl;
    return t;
}

void delete_midi(doremir_midi_t midi)
{
    doremir_delete(midi);
}


// --------------------------------------------------------------------------------

/**
    Creates a simple message from the given components.
    @param status   The status byte.
    @param fst      The first data byte.
    @param snd      The second data byte.
    @return         A new Midi message.
 */
doremir_midi_t doremir_midi_create_simple(status_t status,
                                          data_t fst,
                                          data_t snd)
{
    doremir_midi_t m = new_midi();
    m->is_sysex = false;
    m->data.simple[0] = status;
    m->data.simple[1] = fst;
    m->data.simple[2] = snd;
    return m;
}

/**
    Creates a sysex message from the given data buffer (not including F7 and FF).
    @param data     Raw data buffer (transfered).
    @return         A new sysex message.
 */
doremir_midi_t doremir_midi_create_sysex(doremir_buffer_t data)
{
    doremir_midi_t m = new_midi();
    m->is_sysex = true;
    m->data.sysex = data;
    return m;
}

doremir_midi_t doremir_midi_copy(doremir_midi_t midi)
{
    doremir_midi_t m = new_midi();
    m->is_sysex = midi->is_sysex;
    if (!midi->is_sysex)
    {
        m->data.simple[0] = midi->data.simple[0];
        m->data.simple[1] = midi->data.simple[1];
        m->data.simple[2] = midi->data.simple[2];
    }
    else
    {
        m->data.sysex = doremir_copy(midi->data.sysex);
    }
    return m;
}

void doremir_midi_destroy(doremir_midi_t midi)
{
    if (midi->is_sysex)
        doremir_destroy(midi->data.sysex);
    delete_midi(midi);
}


// --------------------------------------------------------------------------------

bool midi_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    assert(false && "Not implemented.");
}

bool midi_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    assert(false && "Not implemented.");
}

bool midi_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    assert(false && "Not implemented.");
}

doremir_string_t midi_show(doremir_ptr_t a)
{
    doremir_midi_t midi = (doremir_midi_t) a;
    string_t s = string("<Midi");

    if (!midi->is_sysex)
    {
        s = sdappend(s, doremir_string_format_integer(" %02x", midi->data.simple[0]));
        s = sdappend(s, doremir_string_format_integer(" %02x", midi->data.simple[1]));
        s = sdappend(s, doremir_string_format_integer(" %02x", midi->data.simple[2]));
    }
    else
    {
        // TODO dump without <Buffer > wrap
        s = sdappend(s, string(" SysEx "));
        s = sdappend(s, doremir_show(midi->data.sysex));
        s = sdappend(s, string(" "));
    }

    s = sdappend(s, string(">"));
    return s;
}

doremir_ptr_t midi_copy(doremir_ptr_t a)
{
    return doremir_midi_copy(a);
}

void midi_destroy(doremir_ptr_t a)
{
    doremir_midi_destroy(a);
}


doremir_ptr_t midi_impl(doremir_id_t interface)
{
    static doremir_equal_t midi_equal_impl = { midi_equal };
    static doremir_order_t midi_order_impl = { midi_less_than, midi_greater_than };
    static doremir_string_show_t midi_show_impl = { midi_show };
    static doremir_copy_t midi_copy_impl = { midi_copy };
    static doremir_destroy_t midi_destroy_impl = { midi_destroy };

    switch (interface)
    {
    case doremir_equal_i:
        return &midi_equal_impl;

    case doremir_order_i:
        return &midi_order_impl;

    case doremir_string_show_i:
        return &midi_show_impl;

    case doremir_copy_i:
        return &midi_copy_impl;

    case doremir_destroy_i:
        return &midi_destroy_impl;

    default:
        return NULL;
    }
}
