
#pragma once

#include <utility>
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
        : public processor<unit, unit, unit,
                           unit, unit,
                           A, A>
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
      
      void* dynamic_identity_processor(audio_type type)
      {                       
        using tag = audio_type_tag;
        if (type.tag == tag::sample32)
          return new identity_processor<sample32>;

        if (type.tag == tag::sample64)
          return new identity_processor<sample64>;

        if (type.tag == tag::sample32)
          return new identity_processor<sample32>;
      }

    }
  }
}

