
#pragma once

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
          buffer = new char[size];
        }
        void cleanup(intptr_t argument)
        {
          delete buffer;
        }
        void load(intptr_t argument) {}
        void store(intptr_t argument) {}
        bool is_ready()
        {
          return true;
        }
        void process(intptr_t input_messages,
                     intptr_t input,
                     intptr_t output,
                     intptr_t output_messages)
        {
          std::copy(input, size, buffer);
          std::copy(buffer, size, output);
        }
      };

    }
  }
}

