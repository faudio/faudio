
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  namespace audio
  {
    /**
      Synopsis:

          concept Stream<typename X>
            : Startable<X>
            , Semigroup<X>
            , Addressable<X>
          {
            typename session_type;
            typename device_group_type;
            typename device_type;
            typename time_type;
            typename processor_type;
            typename result_type = processor_type::result_type;

            requires (Session<session_type>);
            requires (DeviceGroup<device_group_type>);
            requires (Device<device_type>);

                                  X::X(device_type& in, processor_type& proc, device_type& out);
                                  ~X::X();
            improving<time_type>  X::time();
            time_type             X::length();
            improving<time_type>  X::progress(); // progress() == time() / length()
            future<result_type>   X::result();
          }

      Models:
        none
    */
    template <class X>
    struct Stream
        : Startable<X>
        , Semigroup<X>
        , Addressable<X>
    {
      BOOST_CONCEPT_USAGE(Stream)
      {
      }
    private:
      X t;
    };
  }
}

