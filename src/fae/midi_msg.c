
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/midi_msg.h>
#include <fae/util.h>

typedef fae_midi_msg_status_t   status_t;
typedef fae_midi_msg_data_t     data_t;

struct _fae_midi_msg_t {
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

inline static fae_midi_msg_t new_midi_msg()
{
    fae_ptr_t midi_msg_impl(fae_id_t interface);

    fae_midi_msg_t t = fae_new(midi_msg);
    t->impl  = &midi_msg_impl;
    return t;
}

void delete_midi_msg(fae_midi_msg_t midi)
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
fae_midi_msg_t fae_midi_msg_create_simple(status_t status,
                                          data_t data1,
                                          data_t data2)
{
    assert(status != 0xf0 && status != 0xf7);

    fae_midi_msg_t m = new_midi_msg();

    m->is_sysex = false;
    char simp[3] = { status, data1, data2 };
    memcpy(&m->data.simple, simp, 3);

    return m;
}

/** Creates a sysex message from the given data buffer (not including F0 and F7).
    @param data     Raw data buffer (transfered).
    @return         A new sysex message.
 */
fae_midi_msg_t fae_midi_msg_create_sysex(fae_buffer_t data)
{
    fae_midi_msg_t m = new_midi_msg();
    m->is_sysex = true;
    m->data.sysex = data;
    return m;
}

/** Copy the given midi message.
 */
fae_midi_msg_t fae_midi_msg_copy(fae_midi_msg_t midi_msg)
{
    fae_midi_msg_t m = new_midi_msg();
    m->is_sysex = midi_msg->is_sysex;

    if (!midi_msg->is_sysex) {
        memcpy(m->data.simple, midi_msg->data.simple, 3);
    } else {
        m->data.sysex = fae_copy(midi_msg->data.sysex);
    }

    return m;
}

/** Destroy the given midi_msg message.
 */
void fae_midi_msg_destroy(fae_midi_msg_t midi_msg)
{
    if (midi_msg->is_sysex) {
        fae_destroy(midi_msg->data.sysex);
    }

    delete_midi_msg(midi_msg);
}

/** Return the status byte of given midi_msg message.
 */
bool fae_midi_msg_is_simple(fae_midi_msg_t midi_msg)
{
    return !midi_msg->is_sysex;
}

/** Return whether the given midi_msg message is a sysex message.
 */
bool fae_midi_msg_is_sysex(fae_midi_msg_t midi_msg)
{
    return midi_msg->is_sysex;
}

/** Return the status byte of given midi_msg message.
 */
fae_midi_msg_status_t fae_midi_msg_status(fae_midi_msg_t midi_msg)
{
    assert(is_simple(midi_msg) && "Not a simple message");
    return midi_msg->data.simple[0] & 0xf0;
}

/** Return the channel byte of given midi_msg message.
 */
fae_midi_msg_channel_t fae_midi_msg_channel(fae_midi_msg_t midi_msg)
{
    assert(is_simple(midi_msg) && "Not a simple message");
    return midi_msg->data.simple[0] & 0x0f;
}

/** Return whether the given midi_msg message is a non-sysex message.
 */
fae_pair_t fae_midi_msg_simple_data(fae_midi_msg_t midi_msg)
{
    assert(is_simple(midi_msg) && "Not a simple message");
    return fae_pair_create(i8(midi_msg->data.simple[1]), i8(midi_msg->data.simple[2]));
}

/** Return the data buffer of a sysex message, except for the wrapping `F0` and `F7` bytes.
 */
fae_buffer_t fae_midi_msg_sysex_data(fae_midi_msg_t midi_msg)
{
    assert(is_sysex(midi_msg) && "Not a sysex message");
    return midi_msg->data.sysex;
}

#define midi_msg_wrap(status, data1, data2)   \
         ((((data2) << 16) & 0xFF0000) |  \
          (((data1) << 8) & 0xFF00) |     \
          ((status) & 0xFF))

long fae_midi_msg_simple_to_long(fae_midi_msg_t midi_msg)
{
    assert(is_simple(midi_msg) && "Not a simple message");
    return midi_msg_wrap(midi_msg->data.simple[0], midi_msg->data.simple[1], midi_msg->data.simple[2]);
}


// --------------------------------------------------------------------------------

bool midi_msg_equal(fae_ptr_t a, fae_ptr_t b)
{
    midi_msg_t midi_msg1 = (midi_msg_t) a;
    midi_msg_t midi_msg2 = (midi_msg_t) b;

    if (midi_msg1->is_sysex != midi_msg2->is_sysex) {
        return false;
    }

    if (!midi_msg1->is_sysex) {
        return memcmp(&midi_msg1->data.simple, &midi_msg2->data.simple, 3) == 0;
    } else {
        return fae_equal(midi_msg1->data.sysex, midi_msg2->data.sysex);
    }
}

// Note: We arbitrarily define simple < sysex

bool midi_msg_less_than(fae_ptr_t a, fae_ptr_t b)
{
    midi_msg_t midi_msg1 = (midi_msg_t) a;
    midi_msg_t midi_msg2 = (midi_msg_t) b;

    if (midi_msg1->is_sysex != midi_msg2->is_sysex) {
        return midi_msg1->is_sysex;
    }

    if (!midi_msg1->is_sysex) {
        return memcmp(&midi_msg1->data.simple, &midi_msg2->data.simple, 3) < 0;
    } else {
        return fae_less_than(midi_msg1->data.sysex, midi_msg2->data.sysex);
    }
}

bool midi_msg_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    midi_msg_t midi_msg1 = (midi_msg_t) a;
    midi_msg_t midi_msg2 = (midi_msg_t) b;

    if (midi_msg1->is_sysex != midi_msg2->is_sysex) {
        return midi_msg2->is_sysex;
    }

    if (!midi_msg1->is_sysex) {
        return memcmp(&midi_msg1->data.simple, &midi_msg2->data.simple, 3) > 0;
    } else {
        return fae_greater_than(midi_msg1->data.sysex, midi_msg2->data.sysex);
    }
}

fae_string_t midi_msg_show(fae_ptr_t a)
{
    fae_midi_msg_t midi_msg = (fae_midi_msg_t) a;
    string_t s = string("<Midi");

    if (!midi_msg->is_sysex) {
        s = string_dappend(s, fae_string_format_integral(" %02x", midi_msg->data.simple[0]));
        s = string_dappend(s, fae_string_format_integral(" %02x", midi_msg->data.simple[1]));
        s = string_dappend(s, fae_string_format_integral(" %02x", midi_msg->data.simple[2]));
    } else {
        // TODO dump without <Buffer > wrap
        s = string_dappend(s, string(" SysEx "));
        s = string_dappend(s, fae_string_show(midi_msg->data.sysex));
        s = string_dappend(s, string(" "));
    }

    s = string_dappend(s, string(">"));
    return s;
}

fae_ptr_t midi_msg_copy(fae_ptr_t a)
{
    return fae_midi_msg_copy(a);
}

void midi_msg_destroy(fae_ptr_t a)
{
    fae_midi_msg_destroy(a);
}


fae_ptr_t midi_msg_impl(fae_id_t interface)
{
    static fae_equal_t midi_msg_equal_impl = { midi_msg_equal };
    static fae_order_t midi_msg_order_impl = { midi_msg_less_than, midi_msg_greater_than };
    static fae_string_show_t midi_msg_show_impl = { midi_msg_show };
    static fae_copy_t midi_msg_copy_impl = { midi_msg_copy };
    static fae_destroy_t midi_msg_destroy_impl = { midi_msg_destroy };

    switch (interface) {
    case fae_equal_i:
        return &midi_msg_equal_impl;

    case fae_order_i:
        return &midi_msg_order_impl;

    case fae_string_show_i:
        return &midi_msg_show_impl;

    case fae_copy_i:
        return &midi_msg_copy_impl;

    case fae_destroy_i:
        return &midi_msg_destroy_impl;

    default:
        return NULL;
    }
}

