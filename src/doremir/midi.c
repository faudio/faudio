
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/midi.h>
#include <doremir/util.h>

doremir_ptr_t midi_impl(doremir_id_t interface);

typedef doremir_midi_status_t   status_t;
typedef doremir_midi_data_t     data_t;

struct _doremir_midi_t {
  impl_t                  impl;           //  Interface dispatcher
  bool                    is_sysex;       //  Whether it is a sysex message
  union {                                 //  Status and data
    uint8_t               simple[3];
    doremir_buffer_t      sysex;
  } data;
};

inline static doremir_midi_t new_midi()
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
    @param data1    The first data byte.
    @param data2    The second data byte.
    @return         A new Midi message.
 */
doremir_midi_t doremir_midi_create_simple(status_t status,
    data_t data1,
    data_t data2)
{
  assert(status != 0xf0 && status != 0xf7);

  doremir_midi_t m = new_midi();

  m->is_sysex = false;
  m->data.simple[0] = status;
  m->data.simple[1] = data1;
  m->data.simple[2] = data2;

  return m;
}

/**
    Creates a sysex message from the given data buffer (not including F0 and F7).
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

/**
    Copy the given midi message.
 */
doremir_midi_t doremir_midi_copy(doremir_midi_t midi)
{
  doremir_midi_t m = new_midi();
  m->is_sysex = midi->is_sysex;

  if (!midi->is_sysex) {
    m->data.simple[0] = midi->data.simple[0];
    m->data.simple[1] = midi->data.simple[1];
    m->data.simple[2] = midi->data.simple[2];
  } else {
    m->data.sysex = doremir_copy(midi->data.sysex);
  }

  return m;
}

/**
    Destroy the given midi message.
 */
void doremir_midi_destroy(doremir_midi_t midi)
{
  if (midi->is_sysex) {
    doremir_destroy(midi->data.sysex);
  }

  delete_midi(midi);
}

/**
    Return the status byte of given midi message.
 */
doremir_midi_status_t doremir_midi_status(doremir_midi_t midi)
{
  return midi->data.simple[0] & 0x0f;
}

/**
    Return the channel byte of given midi message.
 */
doremir_midi_channel_t doremir_midi_channel(doremir_midi_t midi)
{
  return midi->data.simple[0] & 0xf0;
}

/**
    Return the status byte of given midi message.
 */
bool doremir_midi_is_simple(doremir_midi_t midi)
{
  return !midi->is_sysex;
}

/**
    Return whether the given midi message is a non-sysex message.
 */
doremir_pair_t doremir_midi_simple_data(doremir_midi_t midi)
{
  return doremir_pair_create(i8(midi->data.simple[1]), i8(midi->data.simple[2]));
}

/**
    Return whether the given midi message is a sysex message.
 */
bool doremir_midi_is_sysex(doremir_midi_t midi)
{
  return midi->is_sysex;
}

/**
    Return the data buffer of a sysex message, except for the wrapping `F0` and `F7` bytes.
 */
doremir_buffer_t doremir_midi_sysex_data(doremir_midi_t midi)
{
  return midi->data.sysex;
}


// --------------------------------------------------------------------------------

bool midi_equal(doremir_ptr_t a, doremir_ptr_t b)
{
  midi_t midi1 = (midi_t) a;
  midi_t midi2 = (midi_t) b;

  if (midi1->is_sysex != midi2->is_sysex) {
    return false;
  }

  if (!midi1->is_sysex) {
    return memcmp(&midi1->data.simple, &midi2->data.simple, 3) == 0;
  } else {
    return doremir_equal(midi1->data.sysex, midi2->data.sysex);
  }
}

// Note: We assume simple < sysex

bool midi_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
  midi_t midi1 = (midi_t) a;
  midi_t midi2 = (midi_t) b;

  if (midi1->is_sysex != midi2->is_sysex) {
    return midi1->is_sysex;
  }

  if (!midi1->is_sysex) {
    return memcmp(&midi1->data.simple, &midi2->data.simple, 3) < 0;
  } else {
    return doremir_less_than(midi1->data.sysex, midi2->data.sysex);
  }
}

bool midi_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
  midi_t midi1 = (midi_t) a;
  midi_t midi2 = (midi_t) b;

  if (midi1->is_sysex != midi2->is_sysex) {
    return midi2->is_sysex;
  }

  if (!midi1->is_sysex) {
    return memcmp(&midi1->data.simple, &midi2->data.simple, 3) > 0;
  } else {
    return doremir_greater_than(midi1->data.sysex, midi2->data.sysex);
  }
}

doremir_string_t midi_show(doremir_ptr_t a)
{
  doremir_midi_t midi = (doremir_midi_t) a;
  string_t s = string("<Midi");

  if (!midi->is_sysex) {
    s = string_dappend(s, doremir_string_format_integer(" %02x", midi->data.simple[0]));
    s = string_dappend(s, doremir_string_format_integer(" %02x", midi->data.simple[1]));
    s = string_dappend(s, doremir_string_format_integer(" %02x", midi->data.simple[2]));
  } else {
    // TODO dump without <Buffer > wrap
    s = string_dappend(s, string(" SysEx "));
    s = string_dappend(s, doremir_string_show(midi->data.sysex));
    s = string_dappend(s, string(" "));
  }

  s = string_dappend(s, string(">"));
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

  switch (interface) {
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

