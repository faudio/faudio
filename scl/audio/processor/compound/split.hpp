
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






      // (a ~> (a,a))
      class raw_split_processor : public raw_processor
      {
      public:
        raw_split_processor(size_t size)
          : raw_processor(0, 0, 0,
                          size,
                          size * 2) {}

        void load(intptr_t state) {}
        void store(intptr_t state) {}

        void prepare(intptr_t arg) {}
        void cleanup(intptr_t res) {}

        bool is_ready()
        {
          return true;
        }

        void process(intptr_t in_msg,
                     intptr_t input,
                     intptr_t output,
                     intptr_t out_msg)
        {
          size_t size = raw_processor::input_size;
          scl::raw_copy(input, input + size, output);
          scl::raw_copy(input, input + size, output + size);
        }
      };


    }
  }
}

