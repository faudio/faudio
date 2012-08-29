
#pragma once

#include <list>
#include <memory>
#include <scl/exception.hpp>
#include <scl/audio/audio_types.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      template < class State, class Argument, class Result,
               class MessageInput, class MessageOutput, class Input, class Output >
      struct processor
      {
        using state_type = State;
        using argument_type = Argument;
        using result_type = Result;
        using message_input_type = MessageInput;
        using message_output_type = MessageOutput;
        using input_type = Input;
        using output_type = Output;
      };

      // template < class State, class Argument, class Result,
      //          class MessageInput, class MessageOutput, class Input, class Output >
      // struct dynamic_processor
      // {
      //   using state_type = State;
      //   using argument_type = Argument;
      //   using result_type = Result;
      //   using message_input_type = MessageInput;
      //   using message_output_type = MessageOutput;
      //   using input_type = Input;
      //   using output_type = Output;
      //
      //   virtual ~dynamic_processor() {}
      //   virtual void prepare(const argument_type&) = 0;
      //   virtual void cleanup(result_type&) = 0;
      //   virtual void load(const state_type&) = 0;
      //   virtual void store(state_type&) = 0;
      //   virtual bool is_ready() = 0;
      //   virtual void process(const std::list<message_input_type>&,
      //                        const input_type&,
      //                        output_type&,
      //                        std::list<message_output_type>&) = 0;
      // };
      //
      // template <class T>
      // class dynamic_processor_wrapper : public dynamic_processor <
      //   typename T::state_type,
      //   typename T::argument_type,
      //   typename T::result_type,
      //   typename T::message_input_type,
      //   typename T::message_output_type,
      //   typename T::input_type,
      //   typename T::output_type
      //   >
      // {
      //   T x;
      // public:
      //   using state_type = typename T::state_type;
      //   using argument_type = typename T::argument_type;
      //   using result_type = typename T::result_type;
      //   using message_input_type = typename T::message_input_type;
      //   using message_output_type = typename T::message_output_type;
      //   using input_type = typename T::input_type;
      //   using output_type = typename T::output_type;
      //
      //   void prepare(const argument_type& argument)
      //   {
      //     x.prepare(argument);
      //   }
      //   void cleanup(result_type& result)
      //   {
      //     x.cleanup(result);
      //   }
      //   void load(const state_type& state)
      //   {
      //     x.load(state);
      //   }
      //   void store(state_type& state)
      //   {
      //     x.store(state);
      //   }
      //   bool is_ready()
      //   {
      //     return x.is_ready();
      //   }
      //   void process(const std::list<message_input_type>& message_inputs,
      //                const input_type& input,
      //                output_type& output,
      //                std::list<message_output_type>& message_outputs)
      //   {
      //     x.process(message_inputs, input, output, message_outputs);
      //   }
      // };
      //
      // template < class State, class Argument, class Result,
      //          class MessageInput, class MessageOutput, class Input, class Output >
      // class any_processor
      //   : public processor < State,
      //     Argument,
      //     Result,
      //     MessageInput,
      //     MessageOutput,
      //     Input,
      //     Output >
      // {
      //   using dynamic_processor_type =
      //     dynamic_processor <
      //     State, Argument, Result,
      //     MessageInput, MessageOutput, Input, Output >;
      //
      //   std::shared_ptr<dynamic_processor_type> x;
      //   inline void check()
      //   {
      //     if (!x) throw bad_state();
      //   }
      // public:
      //   using state_type = State;
      //   using argument_type = Argument;
      //   using result_type = Result;
      //   using message_input_type = MessageInput;
      //   using message_output_type = MessageOutput;
      //   using input_type = Input;
      //   using output_type = Output;
      //
      //   void prepare(const argument_type& argument)
      //   {
      //     check();
      //     x->prepare(argument);
      //   }
      //   void cleanup(result_type& result)
      //   {
      //     check();
      //     x->cleanup(result);
      //   }
      //   void load(const state_type& state)
      //   {
      //     check();
      //     x->load(state);
      //   }
      //   void store(state_type& state)
      //   {
      //     check();
      //     x->store(state);
      //   }
      //   bool is_ready()
      //   {
      //     check();
      //     return x.is_ready();
      //   }
      //   void process(const std::list<message_input_type>& message_inputs,
      //                const input_type& input,
      //                output_type& output,
      //                std::list<message_output_type>& message_outputs)
      //   {
      //     check();
      //     x->process(message_inputs, input, output, message_outputs);
      //   }
      // };










      class raw_processor
      {
      public:
        virtual ~raw_processor() {}
        virtual audio_type input_type() = 0;
        virtual audio_type output_type() = 0;
        virtual void prepare(ptr_t argument) = 0;
        virtual void cleanup(ptr_t argument) = 0;
        virtual void load(ptr_t argument) = 0;
        virtual void store(ptr_t argument) = 0;
        virtual bool is_ready() = 0;
        virtual void process(ptr_t input_messages,
                             ptr_t input_data,
                             ptr_t output_data,
                             ptr_t output_messages) = 0;
      };

      using raw_processor_ptr = std::shared_ptr<raw_processor>;
    }
  }
}

