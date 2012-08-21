
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Device<typename X>
          {
            typename session_type;
            typename stream_type;
            typename input_type;
            typename output_type;
            typename name_type = string;
            typename info_type = void;
            name_type    X::name();
            info_type    X::info();
            session_type X::session();
            bool         X::is_valid();
            bool         X::is_active();
          }

      ### Models ###

      (none)
  */

  template <class X>
  struct Device
  {
    BOOST_CONCEPT_USAGE(Device)
    {
    }
  private:
    X t;
  };
}
