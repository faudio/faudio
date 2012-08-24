
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
        raw_processor_ptr x, y;
      public:
        using parent_type = raw_processor;

        raw_parallel_processor(raw_processor_ptr x,
                               raw_processor_ptr y)
          
          : raw_processor(0, 0, 0,
                          x->input_size + y->input_size,
                          x->output_size + y->output_size)
          , x(x)
          , y(y) {}

        void load(intptr_t state) {}
        void store(intptr_t state) {}

        void prepare(intptr_t arg)
        {
          x->prepare(arg);
          y->prepare(arg); // TODO in parallel
        }

        void cleanup(intptr_t res)
        {
          x->cleanup(res);
          y->cleanup(res); // TODO in parallel
        }

        bool is_ready()
        {
          return x->is_ready() && y->is_ready();
        }

        void process(intptr_t in_msg, intptr_t input, intptr_t output, intptr_t out_msg)
        {        
          size_t input2  = input  + x->input_size;
          size_t output2 = output + x->output_size;
          x->process(in_msg, input,  output,  out_msg);
          y->process(in_msg, input2, output2, out_msg);
        }
      };

    }
  }
}

