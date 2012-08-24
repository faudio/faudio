
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
      using unit = std::nullptr_t;

      template <class A>
      class identity_processor
        : public processor <
        unit, unit, unit,
        unit, unit,
        A, A >
      {
      public:
        identity_processor() = default;

        unit prepare(const unit& argument)
        {
        }
        unit cleanup(unit& result)
        {
        }
        unit load(const unit& state)
        {
        }
        unit store(unit& state)
        {
        }
        bool is_ready()
        {
          return true;
        }
        unit process(const std::list<unit>& input_messages,
                     const A& input,
                     A& output,
                     std::list<unit>& output_messages)
        {
          output = input;
        }
      };







      class raw_identity_processor : public raw_processor
      {
      public:
        raw_identity_processor(size_t size)
          : raw_processor(0, 0, 0, size, size) {}
        void prepare(intptr_t argument) {}
        void cleanup(intptr_t argument) {}
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
          size_t size = raw_processor::output_size;
          scl::raw_copy(input, input + size, output);
        }
      };

    }
  }
}

