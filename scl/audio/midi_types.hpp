
namespace scl
{
  namespace audio
  {
    enum midi_status
    {
      midi_note_off         = 0x80;
      midi_note_on          = 0x90;
      midi_after_touch      = 0xa0;
      midi_control_change   = 0xb0;
      midi_program_change   = 0xc0;
      midi_channel_pressure = 0xd0;
      midi_pitch_wheel      = 0xe0;
      midi_sysex            = 0xf0;
    };

    constexpr int midi_status_type(int status);
    constexpr int midi_status_channel(int status);
    constexpr bool midi_status_is_sysex(int status);

    // TODO simple midi functions that works on any Range<Integral>
    // TODO midi file functions that works on any Range<pair<Integral,option<midi_message,midi_sysex_message>>

    // TODO simple wrapper class
    class midi_message
    {
    };

    class midi_sysex_message
    {
    };




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
      return midi_status_type(status) == midi_status_sysex;
    }
  }
}

