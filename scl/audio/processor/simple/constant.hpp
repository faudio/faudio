
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

      // _ ~> a
      class raw_constant_processor : public raw_processor
      {
        audio_type dummy_type;
        audio_type type;
        raw_buffer buffer;
      public:
        using parent_type = raw_processor;

        raw_constant_processor(audio_type dummy_type, audio_type type)
          : dummy_type(dummy_type), type(type) {}

        audio_type input_type()
        {
          return dummy_type;
        }

        audio_type output_type()
        {
          return type;
        }

        void load(ptr_t state) {}
        void store(ptr_t state) {}

        void prepare(ptr_t arg)
        {
          size_t size = type.size();
          buffer.reset(size);
          scl::raw_copy(arg, arg + size, buffer.begin());
        }

        void cleanup(ptr_t res)
        {
          buffer.clear();
        }

        bool is_ready()
        {
          return buffer.size() > 0;
        }

        void process(ptr_t in_msg,
                     ptr_t input,
                     ptr_t output,
                     ptr_t out_msg)
        {
          scl::raw_copy(buffer.begin(), buffer.end(), output);
        }
      };

      /** @endcond */

      /** 
        ## Description
          Ignores its input and outputs a constant value.
        
        ## Associated types
          ### State
            `unit`
          ### Argument
            Type of the constant value.
          ### Result
            `unit`
          ### Message Input
            `unit`
          ### Message Output
            `unit`
          ### Input
            An arbitrary type.
          ### Output
            Type of the constant value.
       */
      template <class A, class B>
      class constant_processor
        : public processor <
        unit, B, unit,
        unit, unit,
        A, B >
      {
        raw_processor_ptr raw;
      public:
        constant_processor()
          : raw(
            new raw_constant_processor(
              audio_type::get<A>(),
              audio_type::get<B>())) {}

        raw_processor_ptr get_raw()
        {
          return raw;
        }
        void load(const unit& state)
        {
          raw->load((ptr_t) &state);
        }
        void store(unit& state)
        {
          raw->store((ptr_t) &state);
        }
        void prepare(const B& argument)
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

