
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
        raw_processor_ptr g;
        raw_processor_ptr f;
        raw_buffer buffer;
      public:
        using parent_type = raw_processor;

        raw_sequence_processor(raw_processor_ptr g,
                               raw_processor_ptr f)
          : g(g), f(f)
        {
          assert(g->output_type() == f->input_type());
        }

        audio_type input_type()
        {
          return g->input_type();
        }

        audio_type output_type()
        {
          return f->output_type();
        }

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg)
        {
          size_t size = g->output_type().size();
// std::cout << "-------- buffer size: " << size << "\n";
          buffer.reset(size);
          g->prepare(arg); // TODO is this right?
          f->prepare(arg);
        }

        void cleanup(ptr_t res)
        {
          g->cleanup(res); // TODO is this right?
          f->cleanup(res);
        }

        bool is_ready()
        {
          return g->is_ready() && f->is_ready();
        }

        void process(ptr_t in_msg,
                     ptr_t input,
                     ptr_t output,
                     ptr_t out_msg)
        {
          g->process(NULL, input, buffer.begin(), NULL);
          f->process(NULL, buffer.begin(), output, NULL);
        }
      };

      /** @endcond internal */

      // template <class A, class B, class C>
      // class sequence_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     A, C >
      // {
      // public:
      //   sequence_processor(value_type value)
      //     : value(value) {}
      // };

    }
  }
}

