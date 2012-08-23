
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Session<typename X>
          : Abortable<X>
          , Semigroup<X>
        {
          typename device_group_type;
          typename device_type;
          typename stream_type;

          requires (DeviceGroup<device_group_type>);
          requires (Device<device_type>);
          requires (Stream<stream_type>);

                                          X::X();
                                          ~X::X();
          list<device_group_type>         X::device_groups() const;
          pair<device_type, device_type>  X::default_devices() const;
          device_type                     X::default_input_device() const;
          device_type                     X::default_output_device() const;
          bool                            X::is_valid() nothrow const;
        }

    Semantics:
    
    Models:
      portaudio_session,
      sndfile_session,
      any_session
  */
  template <class X>
  struct Session
      : Abortable<X>
      , Semigroup<X>
  {
    BOOST_CONCEPT_USAGE(Session)
    {
    }
  private:
    X t;
  };
}
