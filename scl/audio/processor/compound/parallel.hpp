
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
      
      template <class A, class B, class C, class D>
      class parallel_processor
        : public processor<void, void, void,
                           void, void,
                           pair<A,C>, pair<B,D>>
      {
      public:
        parallel_processor(value_type value)
          : value(value) {}

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
        }
        void process(const list<input_message_type>& input_messages,
                     const input_type& input,
                           output_type& output,
                           list<output_message_type>& output_messages)
        {
        }
        
      private:
      };

    }
  }
}

