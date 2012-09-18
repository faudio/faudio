
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

      // (a ~> b) -> (b ~> c) -> (a ~> c)
      class raw_sequence_processor : public raw_processor
      {
        raw_processor_ptr f;
        raw_processor_ptr g;
        raw_buffer buffer;
      public:
        raw_sequence_processor(raw_processor_ptr f,
                               raw_processor_ptr g)
          : f(f), g(g)
        {
          assert(f->output_type() == g->input_type());
        }

        audio_type input_type()
        {
          return f->input_type();
        }

        audio_type output_type()
        {
          return g->output_type();
        }

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg)
        {
          size_t size = f->output_type().size();
          buffer.reset(size);
          f->prepare(arg);
          g->prepare(arg);
        }

        void cleanup(ptr_t res)
        {
          f->cleanup(res);
          g->cleanup(res);
        }

        bool is_ready()
        {
          return f->is_ready() && g->is_ready();
        }

        void process(ptr_t in_msg,
                     ptr_t input,
                     ptr_t output,
                     ptr_t out_msg)
        {
          f->process(NULL, input, buffer.begin(), NULL);
          g->process(NULL, buffer.begin(), output, NULL);
        }
      };

      /** @endcond */

      /** 
        ## Description
          Runs two processors *f* and *g* in sequence.
        
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
            The input type of *f*.
          ### Output
            The output type of *g*.
       */
      template <class F, class G>
      class sequence_processor
        : public processor < 
          unit, unit, unit,
          unit, unit,
          typename F::input_type, 
          typename G::output_type
          >
          // TODO enable if to assure (F::out ~ G::in)
      {
        raw_processor_ptr raw;
      public:
        sequence_processor(F f, G g)
          : raw(
            new raw_sequence_processor(
              f.get_raw(), 
              g.get_raw())) {}
      };      

    }
  }
}

