
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Addressable<typename X>
        {
          typename address_type;

          requires (EqualityComparable<address_type>);

          list<Dispatcher<T>> X::get_dispatcher(address_type);
        }

    Models:
      portaudio_stream,
      sndfile_stream
  */
  template <class X>
  struct Addressable
  {
    using address_type = typename X::address_type;
    BOOST_CONCEPT_ASSERT((EqualityComparable<address_type>));
    BOOST_CONCEPT_USAGE(Addressable)
    {
      // TODO get_dispatcher
    }
  private:
    X t;
  };
}
