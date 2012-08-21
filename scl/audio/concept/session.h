
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Session<typename X> 
  : Abortable<X>
  , Semigroup<X>
{
  typename device_group_type;
  typename device_type;
  typename stream_type;
                                  X::X();
                                  ~X::X();
  list<device_group_type>         X::device_groups() const;
  pair<device_type, device_type>  X::default_devices() const;
  device_type                     X::default_input_device() const;
  device_type                     X::default_output_device() const;
  bool                            X::is_valid() nothrow const;
  // Models: portaudio_session, sndfile_session, any_session
};;;;

*/
  
  template <class X>
  struct Abortable
  {
    BOOST_CONCEPT_USAGE(Timer)
    {  
    }
  private:
    X t;
  };
}
