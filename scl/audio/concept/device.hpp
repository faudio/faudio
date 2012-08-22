
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Device<typename X>
        {
          typename session_type;
          typename device_group_type;
          typename stream_type;
          typename input_type;
          typename output_type;
          typename name_type = string;
          typename info_type = void;

          requires (Session<session_type>);
          requires (DeviceGroup<device_group_type>);
          requires (Stream<stream_type>);
          requires (EqualityComparable<name_type>);
          requires (Copyable<name_type>);
          requires (Copyable<info_type>);

          name_type    X::name();
          info_type    X::info();
          session_type X::session();
          bool         X::is_valid();
          bool         X::is_active();
        }

    Semantics:
      - `x.name()` The name of the device, convertible to scl::ustring.
      - `x.info()` A structure type, providing device-specific information.
      - `x.session()` The session in which the device was loaded.
      - `x.is_valid()` Equivalent to `x.session().is_valid()`.
      - `x.is_active()` Whether the device has non-aborted streams.

    Models:
      - portaudio_device
      - portmidi_device
      - sndfile_device
  */
  template <class X>
  struct Device
  {
    BOOST_CONCEPT_USAGE(Device)
    {
      using session_type = typename X::session_type;
      using stream_type  = typename X::stream_type;
      using input_type   = typename X::input_type;
      using output_type  = typename X::output_type;
      using name_type    = typename X::name_type;
      using info_type    = typename X::info_type;
      BOOST_CONCEPT_USAGE(Device)
      {
        name_type    name     = x.name();
        info_type    info     = x.info();
        session_type session  = x.session();
        bool         valid    = x.is_valid();
        bool         active   = x.is_active();
      }
    }
  private:
    X x;
  };
}
