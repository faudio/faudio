
#pragma once

#include <algorithm>
#include <scl/utility.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {





      // a ~> a
      class raw_delay_processor : public raw_processor
      {
        raw_buffer buffer;
      public:
        using parent_type = raw_processor;

        raw_delay_processor(size_t size)
          : raw_processor(0, 0, 0, size, size) {}

        void load(intptr_t state) {}
        void store(intptr_t state) {}

        void prepare(intptr_t arg)
        {
          size_t size = parent_type::output_size;
          buffer.resize(size);
        }

        void cleanup(intptr_t res)
        {
          buffer.clear();
        }

        bool is_ready()
        {
          return buffer.size() > 0;
        }

        void process(intptr_t in_msg, intptr_t input, intptr_t output, intptr_t out_msg)
        {
          size_t size = parent_type::output_size;
          scl::raw_copy(buffer.begin(), buffer.begin() + size, output);
          scl::raw_copy(input, input + size, buffer.begin());
        }
      };

    }
  }
}

