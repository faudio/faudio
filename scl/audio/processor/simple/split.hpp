
#pragma once

#include <algorithm>
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

      // a ~> (a,a)
      class raw_split_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;
      public:
        using parent_type = raw_processor;

        raw_split_processor(audio_type type)
          : in_type(type), out_type(audio_type::pair(type, type)) {}

        audio_type input_type()
        {
          return in_type;
        }
        audio_type output_type()
        {
          return out_type;
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
          size_t size   = in_type.size();
          size_t offset = out_type.offset();
          scl::raw_copy(input, input + size, output);
          scl::raw_copy(input, input + size, output + offset);
        }
      };

      /** @endcond */

      /** 
        ## Description
          Duplicates its input.
        
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
            An arbitrary type `A`.
          ### Output
            The type `(A,A)`.
       */
      template <class A>
      class split_processor
        : public processor <
        unit, unit, unit,
        unit, unit,
        A, 
        typename audio_pair<A, A>::type >
      {
        raw_processor_ptr raw;
      public:
        split_processor()
          : raw(
            new raw_split_processor(
              audio_type::get<A>())) {}

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
                     typename audio_pair<A, A>::type& output,
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

