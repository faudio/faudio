
#pragma once

#include <algorithm>
#include <scl/utility.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {






      class raw_delay_processor : public raw_processor
      {
        std::unique_ptr<char> buffer;
      public:
        raw_delay_processor(size_t size)
          : raw_processor(0, 0, 0, size, size)
        {}
        void prepare(intptr_t argument)
        {
          size_t size = raw_processor::output_size;
          buffer.reset(new char[size]);
        }
        void cleanup(intptr_t argument)
        {
          buffer.reset();
        }
        void load(intptr_t argument) {}
        void store(intptr_t argument) {}
        bool is_ready()
        {
          return buffer != nullptr;
        }
        void process(intptr_t input_messages,
                     intptr_t input,
                     intptr_t output,
                     intptr_t output_messages)
        {                           
          size_t size = raw_processor::output_size;
          intptr_t buf = (intptr_t) (buffer.get());
          scl::raw_copy(buf, buf + size, output);
          scl::raw_copy(input, input + size, buf);
        }
      };

    }
  }
}

