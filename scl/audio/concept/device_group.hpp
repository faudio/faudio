
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  namespace audio
  {
    /**
      Synopsis:

          concept DeviceGroup<typename X>
          {
            typename session_type;
            typename device_group_type;
            typename device_type;
            typename stream_type;

            requires (Session<session_type>);
            requires (Device<device_type>);
            requires (Stream<stream_type>);

            pair<list<device_type>,list<device_type>> X::devices() const;
            list<device_type>                         X::input_devices() const;
            list<device_type>                         X::output_devices() const;
          }

      Models:
        none
    */
    template <class X>
    struct DeviceGroup
    {
      BOOST_CONCEPT_USAGE(DeviceGroup)
      {
      }
    private:
      X t;
    };
  }
}

