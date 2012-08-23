
namespace scl
{
  namespace audio
  {
    enum midi_status
    {
      note_off         = 0x80;
      note_on          = 0x90;
      after_touch      = 0xa0;
      control_change   = 0xb0;
      program_change   = 0xc0;
      channel_pressure = 0xd0;
      pitch_wheel      = 0xe0;
      sysex            = 0xf0;
    };

    constexpr int  midi_status_type(int status);
    constexpr int  midi_status_channel(int status);
    constexpr bool midi_status_is_sysex(int status);

    // TODO simple midi functions that works on list<Integral>
    // TODO midi file functions that works on list<pair<Integral,option<midi_message,midi_sysex_message>>

    // TODO simple wrapper class
    class midi_simple_message
    {
      int status_type();
      int status_channel();
      int data1();
      int data2();
    };

    class midi_sysex_message
    {
    };

    using midi_message = variant<midi_simple_message, midi_sysex_message>;








    // Implementation

    constexpr int midi_status_type(int status)
    {
      return status & 0xf0;
    }

    constexpr int midi_status_channel(int status)
    {
      return status & 0x0f;
    }

    constexpr bool midi_status_is_sysex(int status)
    {
      return midi_status_type(status) == midi_status::sysex;
    }
  }
}

