
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
        unit process(const std::list<unit>& in_msg,
                     const A& input,
                     A& output,
                     std::list<unit>& output_messages)
        {
          output = input;
        }
      };






      // a ~> a
      class raw_identity_processor : public raw_processor
      {
      public:
        using parent_type = raw_processor;

        raw_identity_processor(size_t size)
          : raw_processor(0, 0, 0, size, size) {}

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg) {}
        void cleanup(ptr_t res) {}

        bool is_ready()
        {
          return true;
        }

        void process(ptr_t in_msg, ptr_t input, ptr_t output, ptr_t out_msg)
        {
          // size_t size = parent_type::input_size;
          // scl::raw_copy(input, input + size, output);
        }
      };
    }
  }
}

