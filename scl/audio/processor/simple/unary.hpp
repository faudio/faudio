
#pragma once

#include <utility>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      template <class A, class B>
      class binary_processor
        : public processor<void, void, void,
                           void, void,
                           A, B>
      {
      public:
        binary_processor(std::function<C(A,B)> function)
          : function(function) {}

        void prepare(const argument_type& argument)
        {          
        }
        void cleanup(result_type& result)
        {          
        }
        void load(const state_type& state)
        {
        }
        void store(state_type& state)
        {
        }
        bool is_ready()
        {
          return true;
        }
        void process(const list<input_message_type>& input_messages,
                     const input_type& input,
                           output_type& output,
                           list<output_message_type>& output_messages)
        {
          output = function(input.first, input.second);
        }
      private:
        std::function<B(A)> function;
      };

    }
  }
}

