
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
      ### Synopsis ###

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
          }

      ### Models ###

      unary_processor, fluidsynth, any_processor
  */

  template <class X>
  struct Processor : Addressable<X>
  {
    BOOST_CONCEPT_USAGE(Processor)
    {
    }
  private:
    X t;
  };
}
