
#pragma once

#include <algorithm>
#include <utility>
#include <scl/utility.hpp>
#include <scl/audio/audio_types.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      // template <class A, class B, class C, class D>
      // class parallel_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     pair<A, C>, pair<B, D >>
      // {
      // public:
      //   parallel_processor(value_type value)
      //     : value(value) {}
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
      //   }
      //   void process(const list<input_message_type>& input_messages,
      //                const input_type& input,
      //                output_type& output,
      //                list<output_message_type>& output_messages)
      //   {
      //   }
      //
      // private:
      // };




      // (a ~> b) -> (c ~> d) -> ((a,b) ~> (c,d))
      class raw_parallel_processor : public raw_processor
      {
        raw_processor_ptr x;
        raw_processor_ptr y;
      public:
        using parent_type = raw_processor;

        raw_parallel_processor(raw_processor_ptr x,
                               raw_processor_ptr y)
          : x(x), y(y) {}

        audio_type input_type()
        {
          return audio_type::pair(x->input_type(), y->input_type());
        }

        audio_type output_type()
        {
          return audio_type::pair(x->output_type(), y->output_type());
        }

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg)
        {
          x->prepare(arg); // TODO is this right?
          y->prepare(arg);
        }

        void cleanup(ptr_t res)
        {
          x->cleanup(res); // TODO is this right?
          y->cleanup(res);
        }

        bool is_ready()
        {
          return x->is_ready() && y->is_ready();
        }

        void process(ptr_t in_msg,
                     ptr_t input,
                     ptr_t output,
                     ptr_t out_msg)
        {
          ptr_t input2  = input  + input_type().offset();
          ptr_t output2 = output + output_type().offset();
          x->process(NULL, input, output, NULL);
          y->process(NULL, input2, output2, NULL);
        }
      };

    }
  }
}

