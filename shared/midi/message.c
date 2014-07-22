
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/midi/message.h>
#include <fa/dynamic.h>
#include <fa/util.h>

typedef fa_midi_message_status_t   status_t;
typedef fa_midi_message_data_t     data_t;

struct _fa_midi_message_t {
    fa_impl_t           impl;           //    Interface dispatcher
    bool                is_sysex;       //    Whether it is a sysex message
    union {                             //    Status or buffer data
        uint8_t         simple[3];
        fa_buffer_t     sysex;
    } data;
};

#define is_simple(x) (!x->is_sysex)
#define is_sysex(x)  (x->is_sysex)


// --------------------------------------------------------------------------------

inline static fa_midi_message_t new_midi_message()
{
    fa_ptr_t midi_message_impl(fa_id_t interface);

    fa_midi_message_t t = fa_new(midi_message);
    t->impl             = &midi_message_impl;
    return t;
}

void delete_midi_message(fa_midi_message_t midi)
{
    fa_delete(midi);
}


// --------------------------------------------------------------------------------

fa_midi_message_t fa_midi_message_create_simple(status_t status,
                                                data_t data1,
                                                data_t data2)
{
    assert(status != 0xf0 && status != 0xf7);

    fa_midi_message_t m = new_midi_message();

    m->is_sysex = false;
    char simp[3] = { status, data1, data2 };
    memcpy(&m->data.simple, simp, 3);

    return m;
}

fa_midi_message_t fa_midi_message_create_sysex(fa_buffer_t data)
{
    fa_midi_message_t m = new_midi_message();
    m->is_sysex = true;
    m->data.sysex = data;
    return m;
}

fa_midi_message_t fa_midi_message_copy(fa_midi_message_t midi_message)
{
    fa_midi_message_t m = new_midi_message();
    m->is_sysex = midi_message->is_sysex;

    if (!midi_message->is_sysex) {
        memcpy(m->data.simple, midi_message->data.simple, 3);
    } else {
        m->data.sysex = fa_copy(midi_message->data.sysex);
    }

    return m;
}

void fa_midi_message_destroy(fa_midi_message_t midi_message)
{
    if (midi_message->is_sysex) {
        fa_destroy(midi_message->data.sysex);
    }

    delete_midi_message(midi_message);
}

bool fa_midi_message_is_simple(fa_midi_message_t midi_message)
{
    return !midi_message->is_sysex;
}

bool fa_midi_message_is_sysex(fa_midi_message_t midi_message)
{
    return midi_message->is_sysex;
}

void fa_midi_message_decons(fa_midi_message_t midi_message, int *statusCh, int *data1, int *data2)
{
    *statusCh = midi_message->data.simple[0];
    *data1    = midi_message->data.simple[1];
    *data2    = midi_message->data.simple[2];
}


fa_midi_message_status_t fa_midi_message_status(fa_midi_message_t midi_message)
{
    assert(is_simple(midi_message) && "Not a simple message");
    return midi_message->data.simple[0] & 0xf0;
}

fa_midi_message_channel_t fa_midi_message_channel(fa_midi_message_t midi_message)
{
    assert(is_simple(midi_message) && "Not a simple message");
    return midi_message->data.simple[0] & 0x0f;
}

fa_pair_t fa_midi_message_simple_data(fa_midi_message_t midi_message)
{
    assert(is_simple(midi_message) && "Not a simple message");
    return fa_pair_create(fa_i8(midi_message->data.simple[1]), fa_i8(midi_message->data.simple[2]));
}

fa_buffer_t fa_midi_message_sysex_data(fa_midi_message_t midi_message)
{
    assert(is_sysex(midi_message) && "Not a sysex message");
    return midi_message->data.sysex;
}

#define midi_message_wrap(status, data1, data2) \
    ((((data2) << 16) & 0xFF0000) | \
    (((data1) << 8) & 0xFF00) |     \
    ((status) & 0xFF))

long fa_midi_message_simple_to_long(fa_midi_message_t midi_message)
{
    assert(is_simple(midi_message) && "Not a simple message");

    return midi_message_wrap(
               midi_message->data.simple[0],
               midi_message->data.simple[1],
               midi_message->data.simple[2]
           );
}


// --------------------------------------------------------------------------------

bool midi_message_equal(fa_ptr_t a, fa_ptr_t b)
{
    fa_midi_message_t midi_message1 = (fa_midi_message_t) a;
    fa_midi_message_t midi_message2 = (fa_midi_message_t) b;

    if (midi_message1->is_sysex != midi_message2->is_sysex) {
        return false;
    }

    if (!midi_message1->is_sysex) {
        return memcmp(&midi_message1->data.simple, &midi_message2->data.simple, 3) == 0;
    } else {
        return fa_equal(midi_message1->data.sysex, midi_message2->data.sysex);
    }
}

// Note: We arbitrarily define simple < sysex

bool midi_message_less_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_midi_message_t midi_message1 = (fa_midi_message_t) a;
    fa_midi_message_t midi_message2 = (fa_midi_message_t) b;

    if (midi_message1->is_sysex != midi_message2->is_sysex) {
        return midi_message1->is_sysex;
    }

    if (!midi_message1->is_sysex) {
        return memcmp(&midi_message1->data.simple, &midi_message2->data.simple, 3) < 0;
    } else {
        return fa_less_than(midi_message1->data.sysex, midi_message2->data.sysex);
    }
}

bool midi_message_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_midi_message_t midi_message1 = (fa_midi_message_t) a;
    fa_midi_message_t midi_message2 = (fa_midi_message_t) b;

    if (midi_message1->is_sysex != midi_message2->is_sysex) {
        return midi_message2->is_sysex;
    }

    if (!midi_message1->is_sysex) {
        return memcmp(&midi_message1->data.simple, &midi_message2->data.simple, 3) > 0;
    } else {
        return fa_greater_than(midi_message1->data.sysex, midi_message2->data.sysex);
    }
}

fa_string_t midi_message_show(fa_ptr_t a)
{
    fa_midi_message_t midi_message = (fa_midi_message_t) a;
    fa_string_t s = fa_string("<Midi");

    if (!midi_message->is_sysex) {
        s = fa_string_dappend(s, fa_string_format_integral(" %02x", midi_message->data.simple[0]));
        s = fa_string_dappend(s, fa_string_format_integral(" %02x", midi_message->data.simple[1]));
        s = fa_string_dappend(s, fa_string_format_integral(" %02x", midi_message->data.simple[2]));
    } else {
        s = fa_string_dappend(s, fa_string(" SysEx "));
        s = fa_string_dappend(s, fa_string_show(midi_message->data.sysex));
        s = fa_string_dappend(s, fa_string(" "));
    }

    s = fa_string_dappend(s, fa_string(">"));
    return s;
}

fa_ptr_t midi_message_copy(fa_ptr_t a)
{
    return fa_midi_message_copy(a);
}

void midi_message_destroy(fa_ptr_t a)
{
    fa_midi_message_destroy(a);
}

fa_dynamic_type_repr_t midi_message_get_type(fa_ptr_t a)
{
    return midi_message_type_repr;
}


fa_ptr_t midi_message_impl(fa_id_t interface)
{
    static fa_equal_t midi_message_equal_impl = { midi_message_equal };
    static fa_order_t midi_message_order_impl = { midi_message_less_than, midi_message_greater_than };
    static fa_string_show_t midi_message_show_impl = { midi_message_show };
    static fa_copy_t midi_message_copy_impl = { midi_message_copy };
    static fa_destroy_t midi_message_destroy_impl = { midi_message_destroy };
    static fa_dynamic_t midi_message_dynamic_impl = { midi_message_get_type };

    switch (interface) {
    case fa_equal_i:
        return &midi_message_equal_impl;

    case fa_order_i:
        return &midi_message_order_impl;

    case fa_string_show_i:
        return &midi_message_show_impl;

    case fa_copy_i:
        return &midi_message_copy_impl;

    case fa_destroy_i:
        return &midi_message_destroy_impl;

    case fa_dynamic_i:
        return &midi_message_dynamic_impl;

    default:
        return NULL;
    }
}

