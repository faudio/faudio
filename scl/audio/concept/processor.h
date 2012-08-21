
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /*
  concept Processor<typename X> : Addressable<X>
  {
    typename tags_type;
    typename state_type;
    typename argument_type;
    typename result_type;
    typename input_message_type;
    typename output_message_type;
    typename input_type;
    typename output_type;
    void X::prepare(const argument_type&);
    void X::cleanup(result_type&);
    void X::load(const state_type&);
    void X::store(state_type&);
    bool X::is_ready();
    void X::process(const list<input_message_type>&,
                    const input_type&,
                          output_type&,
                          list<output_message_type>&);
    // Models: unary_processor, fluidsynth, any_processor
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
