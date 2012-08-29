
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






      // a ~> (a,a)
      class raw_split_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;
      public:
        using parent_type = raw_processor;

        raw_split_processor(audio_type type)
          : in_type(type), out_type(audio_type::pair(type, type)) {}

        audio_type input_type()
        {
          return in_type;
        }
        audio_type output_type()
        {
          return out_type;
        }

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg) {}
        void cleanup(ptr_t res) {}

        bool is_ready()
        {
          return true;
        }

        void process(ptr_t in_msg,
                     ptr_t input,
                     ptr_t output,
                     ptr_t out_msg)
        {
          size_t size   = in_type.size();
          size_t offset = out_type.offset();
          scl::raw_copy(input, input + size, output);
          scl::raw_copy(input, input + size, output + offset);
        }
      };
    }
  }
}

