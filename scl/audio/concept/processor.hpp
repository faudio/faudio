
#pragma once

#include <list>
#include <scl/concept.hpp>
#include <scl/audio/concept/addressable.hpp>

namespace scl
{
  namespace audio
  {
    /**
      Synopsis:

          concept Processor<typename X> : Addressable<X>
          {
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

      Semantics:
        - `x.prepare(arg)`
          - Precondition: `x` is not in ready state
          - Postcondition: `x` is in ready state
        - `x.cleanup(res)`
          - Precondition: `x` is in ready state
          - Postcondition: `x` is not in ready state
        - `x.load(state)`
          - Load the state of `x` from `state`
        - `x.store(state)`
          - Store the state of `x` to `state`
        - `x.is_ready()`
          - Whether `x` is in ready state
        - `x.process(in_msgs, in, out, out_msgs)`
          - Precondition: `x` is in ready state
          - Postcondition: `x` is in ready state

      Models:
        - unary_processor,
        - fluidsynth,
        - any_processor
    */
    template <class X>
    struct Processor : Addressable<X>
    {
      using state_type = typename X::state_type;
      using argument_type = typename X::argument_type;
      using result_type = typename X::result_type;
      using input_message_type = typename X::input_message_type;
      using output_message_type = typename X::output_message_type;
      using input_type = typename X::input_type;
      using output_type = typename X::output_type;

      BOOST_CONCEPT_USAGE(Processor)
      {
        x.prepare(arg);
        x.cleanup(res);
        x.load(state);
        x.store(state);
        bool ready = x.is_ready();
        x.process(in_msgs, in, out, out_msgs);
      }
    private:
      X x;
      state_type state;
      argument_type arg;
      result_type res;
      std::list<input_message_type> in_msgs;
      std::list<output_message_type> out_msgs;
      input_type in;
      output_type out;
    };
  }
}

