
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

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg)
        {
          size_t size = parent_type::output_size;
          buffer.resize(size);
        }

        void cleanup(ptr_t res)
        {
          buffer.clear();
        }

        bool is_ready()
        {
          return buffer.size() > 0;
        }

        void process(ptr_t in_msg, ptr_t input, ptr_t output, ptr_t out_msg)
        {
          size_t size = parent_type::output_size;
          scl::raw_copy(buffer.begin(), buffer.begin() + size, output);
          scl::raw_copy(input, input + size, buffer.begin());
        }
      };

    }
  }
}

