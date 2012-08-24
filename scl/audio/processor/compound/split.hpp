
#pragma once

#include <algorithm>
#include <utility>
#include <scl/utility.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      // template <class A>
      // class split_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     A, pair<A, A >>
      // {
      // public:
      //   split_processor() = default;
      // 
      //   void prepare(const argument_type& argument)
      //   {
      //   }
      //   void cleanup(result_type& result)
      //   {
      //   }
      //   void load(const state_type& state)
      //   {
      //   }
      //   void store(state_type& state)
      //   {
      //   }
      //   bool is_ready()
      //   {
      //     return true;
      //   }
      //   void process(const list<input_message_type>& input_messages,
      //                const input_type& input,
      //                output_type& output,
      //                list<output_message_type>& output_messages)
      //   {
      //     output.first  = input;
      //     output.second = input;
      //   }
      // };







      class raw_split_processor : public raw_processor
      {
        std::unique_ptr<char> buffer;
      public:
        raw_split_processor(size_t size)
          : raw_processor(0, 0, 0, size, size * 2) {}
        void prepare(intptr_t argument)
        {
          size_t size = raw_processor::output_size;
          buffer.reset(new char[size]);
        }
        void cleanup(intptr_t argument)
        {
          buffer.reset();
        }
        void load(intptr_t argument) {}
        void store(intptr_t argument) {}
        bool is_ready()
        {
          return buffer != nullptr;
        }
        void process(intptr_t input_messages,
                     intptr_t input,
                     intptr_t output,
                     intptr_t output_messages)
        {
          // std::copy(input, input + size, output);
          // std::copy(input, input + size, output + size);
        }
      };


    }
  }
}

