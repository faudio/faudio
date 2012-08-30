
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
      /** @cond internal */

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

      /** @endcond */

      /** 
        ## Description
          Runs two processors *x* and *y* in parallel.
        
        ## Associated types
          ### State
            `unit`
          ### Argument
            `unit`
          ### Result
            `unit`
          ### Message Input
            `unit`
          ### Message Output
            `unit`
          ### Input
            The pair `(a,b)` where *a* is the input to *x* and *b* is the input to *y*.
          ### Output
            The pair `(c,d)` where *c* is the output of *x* and *d* is the output of *y*.
       */
      
      template <class X, class Y>
      class parallel_processor
        : public processor < 
          unit, unit, unit,
          unit, unit,
          typename audio_pair<typename X::input_type,  typename Y::input_type>::type, 
          typename audio_pair<typename X::output_type, typename Y::output_type>::type
          >
      {
        raw_processor_ptr raw;
      public:
        parallel_processor(X x, Y y)
          : raw(new raw_parallel_processor(x.get_raw(), y.get_raw())) {}
      };      
    }
  }
}

