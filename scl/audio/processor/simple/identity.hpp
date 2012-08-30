
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

      // a ~> a
      class raw_identity_processor : public raw_processor
      {
        audio_type type;
      public:
        using parent_type = raw_processor;

        raw_identity_processor(audio_type type)
          : type(type) {}

        audio_type input_type()
        {
          return type;
        }

        audio_type output_type()
        {
          return type;
        }

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg) {}
        void cleanup(ptr_t res) {}

        bool is_ready()
        {
          return true;
        }

        void process(ptr_t in_msg,
                     ptr_t input,
                     ptr_t output,
                     ptr_t out_msg)
        {
          size_t size = type.size();
          scl::raw_copy(input, input + size, output);
        }
      };


      /** @endcond */

      /** 
        ## Description
          Returns its input unchanged.
        
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
            An arbitrary type.
          ### Output
            Same as the input type.
       */
      template <class A>
      class identity_processor : public processor <
        unit, unit, unit,
        unit, unit,
        A, A >
      {
        raw_processor_ptr raw;
      public:
        identity_processor()
          : raw(
            new raw_identity_processor(
              audio_type::get<A>())) {}

        void load(const unit& state)
        {
          raw->load((ptr_t) &state);
        }
        void store(unit& state)
        {
          raw->store((ptr_t) &state);
        }
        void prepare(const unit& argument)
        {
          raw->prepare((ptr_t) &argument);
        }
        void cleanup(unit& result)
        {
          raw->cleanup((ptr_t) &result);
        }
        bool is_ready()
        {
          return raw->is_ready();
        }
        void process(const std::list<unit>& input_messages,
                     const A& input,
                     A& output,
                     std::list<unit>& output_messages)
        {
          raw->process(
            (ptr_t) &input_messages,
            (ptr_t) &input,
            (ptr_t) &output,
            (ptr_t) &output_messages);
        }
      };

    }
  }
}

