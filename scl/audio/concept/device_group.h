
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /*
  concept DeviceGroup<typename X>
  {
    pair<list<device_type>,list<device_type>> X::devices() const;
    list<device_type>                         X::input_devices() const;
    list<device_type>                         X::output_devices() const;
  }

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
