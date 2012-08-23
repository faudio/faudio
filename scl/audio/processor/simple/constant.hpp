
#pragma once

#include <utility>
#include <scl/audio/audio_types.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      template <class A, class B>
      class constant_processor
        : public processor<void, void, void,
                           void, void,
                           A, B>
      {
      public:        
        constant_processor(output_type value)
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
          return value ? true : false;
        }
        void process(const list<input_message_type>& input_messages,
                     const input_type& input,
                           output_type& output,
                           list<output_message_type>& output_messages)
        {
          output = value;
        }
        
      private:
        output_type value;
      };
      
      any_processor<>
      
    }
  }
}

