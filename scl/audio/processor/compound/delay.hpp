
#pragma once

#include <algorithm>
#include <scl/utility.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {





      // (a ~> a)
      class raw_delay_processor : public raw_processor
      {
        std::unique_ptr<char> buffer;
      public:
        raw_delay_processor(size_t size)
          : raw_processor(0, 0, 0,
                          size, size) {}

        void load(intptr_t state) {}
        void store(intptr_t state) {}

        void prepare(intptr_t arg)
        {
          size_t size = raw_processor::output_size;
          buffer.reset(new char[size]);
        }

        void cleanup(intptr_t res)
        {
          buffer.reset();
        }

        bool is_ready()
        {
          return buffer != nullptr;
        }

        void process(intptr_t in_msg,
                     intptr_t input,
                     intptr_t output,
                     intptr_t out_msg)
        {
          size_t   size = raw_processor::output_size;
          intptr_t buf  = (intptr_t) (buffer.get());
          scl::raw_copy(buf, buf + size, output);
          scl::raw_copy(input, input + size, buf);
        }
      };

    }
  }
}

